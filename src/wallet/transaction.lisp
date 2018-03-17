;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +encrypted-payment-id-tail+ 141)
(define-constant +payment-proof-header+ "ProofV1" :test #'string=)
(define-constant +inbound-transaction-proof-header+ "InProofV1" :test #'string=)
(define-constant +outbound-transaction-proof-header+ "OutProofV1" :test #'string=)

(defun encrypt-payment-id (payment-id public-view-key transaction-secret-key)
  "Encrypt a PAYMENT-ID using a shared secret derived from
a PUBLIC-VIEW-KEY and a TRANSACTION-SECRET-KEY."
  (let* ((derivation (derive-key public-view-key transaction-secret-key))
         (data (concatenate 'octet-vector derivation (vector +encrypted-payment-id-tail+)))
         (key (fast-hash data)))
    (map 'octet-vector #'logxor payment-id key)))

(defun decrypt-payment-id (encrypted-payment-id transaction-public-key secret-view-key)
  "Decrypt an ENCRYPTED-PAYMENT-ID using a shared secret derived-from
a TRANSACTION-PUBLIC-KEY and a SECRET-VIEW-KEY."
  (encrypt-payment-id encrypted-payment-id transaction-public-key secret-view-key))

(defun output-for-address-p (output-key output-index transaction-public-key address secret-view-key)
  "Check if an ADDRESS is the destination of an output."
  (let* ((public-spend-key (geta (decode-address address) :public-spend-key))
         (derivation (derive-key transaction-public-key secret-view-key))
         (key (derive-output-public-key derivation output-index public-spend-key)))
    (equalp key output-key)))

(defun decrypt-amount (encrypted-amount output-index transaction-public-key secret-view-key)
  "Decrypt a transaction output's ENCRYPTED-AMOUNT."
  (let* ((amount (bytes->integer encrypted-amount))
         (derivation (derive-key transaction-public-key secret-view-key))
         (secret (derivation->scalar derivation output-index))
         (amount-mask (bytes->integer (hash-to-scalar (hash-to-scalar secret)))))
    (mod (- amount amount-mask) +l+)))

(defun received-amount (transaction address secret-view-key)
  "Return the total amount that an ADDRESS received in a TRANSACTION."
  (let* ((prefix (geta transaction :prefix))
         (outputs (geta prefix :outputs))
         (extra (geta prefix :extra))
         (transaction-public-key (dolist (field extra)
                                   (let ((key (geta field :transaction-public-key)))
                                     (when key
                                       (return key)))))
         (rct-signature (geta transaction :rct-signature))
         (received 0))
    (dotimes (i (length outputs))
      (let* ((output (aref outputs i))
             (key (geta (geta output :target) :key)))
        (when (output-for-address-p key i transaction-public-key address secret-view-key)
          (let ((amount (if (null rct-signature)
                            (geta output :amount)
                            (let* ((ecdh-info (aref (geta rct-signature :ecdh-info) i))
                                   (encrypted-amount (geta ecdh-info :amount)))
                              (decrypt-amount encrypted-amount
                                              i
                                              transaction-public-key
                                              secret-view-key)))))
            (incf received amount)))))
    received))

(defun prove-payment (transaction-hash address transaction-secret-key)
  "Prove that a payment to an ADDRESS was made."
  (let* ((recipient-public-view-key (geta (decode-address address) :public-view-key))
         (key-derivation (point->bytes (point* (bytes->point recipient-public-view-key)
                                               (bytes->integer transaction-secret-key))))
         (proof-data (generate-transaction-proof transaction-hash
                                                 recipient-public-view-key
                                                 key-derivation
                                                 transaction-secret-key)))
    (concatenate 'string
                 +payment-proof-header+
                 (base58-encode key-derivation)
                 (base58-encode proof-data))))

(defun valid-payment-proof-p (transaction-hash address transaction-public-key proof)
  "Return T if PROOF of transaction to an ADDRESS is valid, and NIL
otherwise."
  (let ((header-length (length +payment-proof-header+))
        (encoded-key-length (base58-encoded-length +key-length+))
        (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length proof) (+ header-length encoded-key-length encoded-signature-length))
               (string= proof +payment-proof-header+ :end1 header-length))
      (let ((recipient-public-view-key (geta (decode-address address) :public-view-key))
            (key-derivation (base58-decode (subseq proof
                                                   header-length
                                                   (+ header-length encoded-key-length))))
            (proof-data (base58-decode (subseq proof (+ header-length encoded-key-length)))))
        (verify-transaction-proof transaction-hash
                                  recipient-public-view-key
                                  key-derivation
                                  transaction-public-key
                                  proof-data)))))

(defun prove-inbound-transaction (transaction-hash address message transaction-public-key secret-view-key)
  "Prove that a transaction was received by an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress (geta address-info :subaddress))
         (public-spend-key (if subaddress
                               (geta address-info :public-spend-key)
                               (point->bytes +g+))))
    (multiple-value-bind (shared-secret proof)
        (generate-inbound-transaction-proof transaction-hash
                                            transaction-public-key
                                            secret-view-key
                                            public-spend-key
                                            message)
      (concatenate 'string
                   +inbound-transaction-proof-header+
                   (base58-encode shared-secret)
                   (base58-encode proof)))))

(defun valid-inbound-transaction-proof-p (transaction-hash address message transaction-public-key proof &optional transaction-secret-key)
  "Return T if a PROOF of inbound transaction for an ADDRESS is valid,
and NIL otherwise."
  (let* ((header-length (length +inbound-transaction-proof-header+))
         (encoded-key-length (base58-encoded-length +key-length+))
         (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length proof) (+ header-length encoded-key-length encoded-signature-length))
               (string= proof +inbound-transaction-proof-header+ :end1 header-length))
      (let* ((address-info (decode-address address))
             (subaddress (geta address-info :subaddress))
             (public-spend-key (if subaddress
                                   (geta address-info :public-spend-key)
                                   (point->bytes +g+)))
             (public-view-key (geta address-info :public-view-key))
             (shared-secret (base58-decode (subseq proof
                                                   header-length
                                                   (+ header-length encoded-key-length))))
             (signature (base58-decode (subseq proof (+ header-length encoded-key-length)))))
        (verify-inbound-transaction-proof transaction-hash
                                          transaction-public-key
                                          public-view-key
                                          public-spend-key
                                          message
                                          shared-secret
                                          signature
                                          transaction-secret-key)))))

(defun prove-outbound-transaction (transaction-hash address message transaction-secret-key)
  "Prove that a transaction was send to an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress (geta address-info :subaddress))
         (public-spend-key (if subaddress
                               (geta address-info :public-spend-key)
                               (point->bytes +g+)))
         (public-view-key (geta address-info :public-view-key)))
    (multiple-value-bind (shared-secret proof)
        (generate-outbound-transaction-proof transaction-hash
                                             transaction-secret-key
                                             public-view-key
                                             public-spend-key
                                             message)
      (concatenate 'string
                   +outbound-transaction-proof-header+
                   (base58-encode shared-secret)
                   (base58-encode proof)))))

(defun valid-outbound-transaction-proof-p (transaction-hash address message transaction-public-key proof &optional secret-view-key)
  "Return T if a PROOF of outbound transaction for an ADDRESS is
valid, and NIL otherwise."
  (let* ((header-length (length +outbound-transaction-proof-header+))
         (encoded-key-length (base58-encoded-length +key-length+))
         (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length proof) (+ header-length encoded-key-length encoded-signature-length))
               (string= proof +outbound-transaction-proof-header+ :end1 header-length))
      (let* ((address-info (decode-address address))
             (subaddress (geta address-info :subaddress))
             (public-spend-key (if subaddress
                                   (geta address-info :public-spend-key)
                                   (point->bytes +g+)))
             (public-view-key (geta address-info :public-view-key))
             (shared-secret (base58-decode (subseq proof
                                                   header-length
                                                   (+ header-length encoded-key-length))))
             (signature (base58-decode (subseq proof (+ header-length encoded-key-length)))))
        (verify-outbound-transaction-proof transaction-hash
                                           transaction-public-key
                                           public-view-key
                                           public-spend-key
                                           message
                                           shared-secret
                                           signature
                                           secret-view-key)))))
