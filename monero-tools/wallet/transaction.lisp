;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +encrypted-payment-id-tail+ 141)
(define-constant +payment-proof-header+ "ProofV1" :test #'string=)
(define-constant +inbound-transaction-proof-v1-header+ "InProofV1" :test #'string=)
(define-constant +outbound-transaction-proof-v1-header+ "OutProofV1" :test #'string=)
(define-constant +inbound-transaction-proof-v2-header+ "InProofV2" :test #'string=)
(define-constant +outbound-transaction-proof-v2-header+ "OutProofV2" :test #'string=)

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

(defun output-for-address-p (output-key output-index transaction-public-key additional-public-keys address secret-view-key)
  "Check if an ADDRESS is the destination of an output."
  (let* ((address-info (decode-address address))
         (use-additional-key (and (> (length additional-public-keys) output-index)
                                  (geta address-info :subaddress)))
         (public-spend-key (geta address-info :public-spend-key))
         (derivation (derive-key (if use-additional-key
                                     (elt additional-public-keys output-index)
                                     transaction-public-key)
                                 secret-view-key))
         (key (derive-output-public-key derivation output-index public-spend-key)))
    (equalp key output-key)))

(defun output-destination-address (output-key output-index transaction-public-key additional-public-keys subaddress-indexes-table secret-view-key &key (chain :mainnet))
  "Return the (sub)address an output is for and a list containing the
subaddress' major index and minor index. If the output is not related to the
SECRET-VIEW-KEY, return NIL and NIL."
  (let* ((use-additional-key (> (length additional-public-keys) output-index))
         (derivation (derive-key (if use-additional-key
                                     (elt additional-public-keys output-index)
                                     transaction-public-key)
                                 secret-view-key))
         (public-spend-key (output-public-key->public-spend-subkey derivation
                                                                   output-index
                                                                   output-key))
         (indexes (gethash public-spend-key subaddress-indexes-table)))
    (when (and use-additional-key (null indexes))
      (setf derivation (derive-key transaction-public-key secret-view-key))
      (setf public-spend-key (output-public-key->public-spend-subkey derivation
                                                                     output-index
                                                                     output-key))
      (setf indexes (gethash public-spend-key subaddress-indexes-table)))
    (if indexes
        (let* ((subaddress-p (notevery #'zerop indexes))
               (public-view-key (if subaddress-p
                                    (public-spend-subkey->public-view-subkey secret-view-key
                                                                             public-spend-key)
                                    (secret-key->public-key secret-view-key)))
               (address (encode-address public-spend-key
                                        public-view-key
                                        :subaddress subaddress-p
                                        :chain chain)))
          (values address indexes))
        (values nil nil))))

(defun decrypt-amount (encrypted-amount output-index transaction-public-key secret-view-key)
  "Decrypt a transaction output's ENCRYPTED-AMOUNT."
  (let* ((amount (bytes->integer encrypted-amount))
         (derivation (derive-key transaction-public-key secret-view-key))
         (secret (derivation->scalar derivation output-index))
         (amount-mask (bytes->integer (hash-to-scalar (hash-to-scalar secret)))))
    (mod (- amount amount-mask) +l+)))

(defun find-extra-field (extra name)
  "Return the value matching a specific NAME in the EXTRA field of
a transaction."
  (dolist (field extra)
    (let ((key (geta field name)))
      (when key
        (return key)))))

(defun received-amount (transaction address secret-view-key)
  "Return the total amount that an ADDRESS received in a TRANSACTION."
  (let* ((prefix (geta transaction :prefix))
         (outputs (geta prefix :outputs))
         (extra (geta prefix :extra))
         (transaction-public-key (find-extra-field extra :transaction-public-key))
         (additional-public-keys (find-extra-field extra :additional-public-keys))
         (rct-signature (geta transaction :rct-signature))
         (use-additional-key (and (= (length additional-public-keys) (length outputs))
                                  (geta (decode-address address) :subaddress)))
         (received 0))
    (dotimes (i (length outputs))
      (let* ((output (aref outputs i))
             (key (geta (geta output :target) :key)))
        (when (output-for-address-p key
                                    i
                                    transaction-public-key
                                    additional-public-keys
                                    address
                                    secret-view-key)
          (let ((amount (if (or (null rct-signature)
                                (eql (geta rct-signature :type) +rct-type-null+))
                            (geta output :amount)
                            (let* ((ecdh-info (aref (geta rct-signature :ecdh-info) i))
                                   (encrypted-amount (geta ecdh-info :amount)))
                              (decrypt-amount encrypted-amount
                                              i
                                              (if use-additional-key
                                                  (elt additional-public-keys i)
                                                  transaction-public-key)
                                              secret-view-key)))))
            (incf received amount)))))
    received))

(defun spent-key-images (transaction)
  "Return the key images matching the real inputs of a TRANSACTION."
  (let ((inputs (geta (geta transaction :prefix) :inputs)))
    (map 'list
         (lambda (input)
           (geta (geta input :key) :key-image))
         inputs)))

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

(defun prove-inbound-transaction-v1 (transaction-hash address message transaction-public-key secret-view-key)
  "Prove that a transaction was received by an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress (geta address-info :subaddress))
         (public-spend-key (if subaddress
                               (geta address-info :public-spend-key)
                               (point->bytes +g+))))
    (multiple-value-bind (shared-secret proof)
        (generate-inbound-transaction-proof-v1 transaction-hash
                                               transaction-public-key
                                               secret-view-key
                                               public-spend-key
                                               message)
      (concatenate 'string
                   +inbound-transaction-proof-v1-header+
                   (base58-encode shared-secret)
                   (base58-encode proof)))))

(defun prove-inbound-transaction-v2 (transaction-hash address message transaction-public-key secret-view-key)
  "Prove that a transaction was received by an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress-p (geta address-info :subaddress))
         (public-view-key (geta address-info :public-view-key))
         (public-spend-key (geta address-info :public-spend-key))
         (transaction-public-keys (split-bytes transaction-public-key +key-length+))
         (proof (generate-inbound-transaction-proof-v2 transaction-hash
                                                       transaction-public-keys
                                                       secret-view-key
                                                       public-view-key
                                                       public-spend-key
                                                       subaddress-p
                                                       message)))
    (concatenate 'string
                 +inbound-transaction-proof-v2-header+
                 (base58-encode proof))))

(defun prove-inbound-transaction (transaction-hash address message transaction-public-key secret-view-key)
  "Prove that a transaction was received by an ADDRESS."
  (prove-inbound-transaction-v2 transaction-hash
                                address
                                message
                                transaction-public-key
                                secret-view-key))

(defun valid-inbound-transaction-proof-v1-p (transaction-hash address message transaction-public-key proof &optional transaction-secret-key)
  "Return T if a PROOF of inbound transaction for an ADDRESS is valid,
and NIL otherwise."
  (let ((header-length (length +inbound-transaction-proof-v1-header+))
        (encoded-key-length (base58-encoded-length +key-length+))
        (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length proof) (+ header-length encoded-key-length encoded-signature-length))
               (string= proof +inbound-transaction-proof-v1-header+ :end1 header-length))
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
        (verify-inbound-transaction-proof-v1 transaction-hash
                                             transaction-public-key
                                             public-view-key
                                             public-spend-key
                                             message
                                             shared-secret
                                             signature
                                             transaction-secret-key)))))

(defun valid-inbound-transaction-proof-v2-p (transaction-hash address message transaction-public-key proof secret-view-key)
  "Return T if a PROOF of inbound transaction for an ADDRESS is valid, and NIL
otherwise."
  (let* ((header-length (length +inbound-transaction-proof-v2-header+))
         (address-info (decode-address address))
         (subaddress-p (geta address-info :subaddress))
         (public-view-key (geta address-info :public-view-key))
         (public-spend-key (geta address-info :public-spend-key))
         (transaction-public-keys (split-bytes transaction-public-key +key-length+))
         (proof (base58-decode (subseq proof header-length))))
    (verify-inbound-transaction-proof-v2 transaction-hash
                                         transaction-public-keys
                                         secret-view-key
                                         public-view-key
                                         public-spend-key
                                         subaddress-p
                                         message
                                         proof)))

(defun valid-inbound-transaction-proof-p (transaction-hash address message transaction-public-key proof secret-view-key)
  "Return T if a PROOF of inbound transaction for an ADDRESS is valid, and NIL
otherwise."
  (let ((header (subseq proof 0 (min (length +inbound-transaction-proof-v2-header+)
                                     (length proof)))))
    (cond
      ((string= header +inbound-transaction-proof-v1-header+)
       (valid-inbound-transaction-proof-v1-p transaction-hash
                                             address
                                             message
                                             transaction-public-key
                                             proof))
      ((string= header +inbound-transaction-proof-v2-header+)
       (valid-inbound-transaction-proof-v2-p transaction-hash
                                             address
                                             message
                                             transaction-public-key
                                             proof
                                             secret-view-key)))))

(defun prove-outbound-transaction-v1 (transaction-hash address message transaction-secret-key)
  "Prove that a transaction was send to an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress (geta address-info :subaddress))
         (public-spend-key (if subaddress
                               (geta address-info :public-spend-key)
                               (point->bytes +g+)))
         (public-view-key (geta address-info :public-view-key)))
    (multiple-value-bind (shared-secret proof)
        (generate-outbound-transaction-proof-v1 transaction-hash
                                                transaction-secret-key
                                                public-view-key
                                                public-spend-key
                                                message)
      (concatenate 'string
                   +outbound-transaction-proof-v1-header+
                   (base58-encode shared-secret)
                   (base58-encode proof)))))

(defun prove-outbound-transaction-v2 (transaction-hash address message transaction-secret-key)
  "Prove that a transaction was send to an ADDRESS."
  (let* ((address-info (decode-address address))
         (subaddress-p (geta address-info :subaddress))
         (public-view-key (geta address-info :public-view-key))
         (public-spend-key (geta address-info :public-spend-key))
         (transaction-secret-keys (split-bytes transaction-secret-key +key-length+))
         (proof (generate-outbound-transaction-proof-v2 transaction-hash
                                                        transaction-secret-keys
                                                        public-view-key
                                                        public-spend-key
                                                        subaddress-p
                                                        message)))
    (concatenate 'string
                 +outbound-transaction-proof-v2-header+
                 (base58-encode proof))))

(defun prove-outbound-transaction (transaction-hash address message transaction-secret-key)
  "Prove that a transaction was send to an ADDRESS."
  (prove-outbound-transaction-v2 transaction-hash
                                 address
                                 message
                                 transaction-secret-key))

(defun valid-outbound-transaction-proof-v1-p (transaction-hash address message transaction-public-key proof &optional secret-view-key)
  "Return T if a PROOF of outbound transaction for an ADDRESS is
valid, and NIL otherwise."
  (let ((header-length (length +outbound-transaction-proof-v1-header+))
        (encoded-key-length (base58-encoded-length +key-length+))
        (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length proof) (+ header-length encoded-key-length encoded-signature-length))
               (string= proof +outbound-transaction-proof-v1-header+ :end1 header-length))
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
        (verify-outbound-transaction-proof-v1 transaction-hash
                                              transaction-public-key
                                              public-view-key
                                              public-spend-key
                                              message
                                              shared-secret
                                              signature
                                              secret-view-key)))))

(defun valid-outbound-transaction-proof-v2-p (transaction-hash address message transaction-public-key proof secret-view-key)
  "Return T if a PROOF of outbound transaction for an ADDRESS is valid, and NIL
otherwise."
  (let* ((header-length (length +outbound-transaction-proof-v2-header+))
         (address-info (decode-address address))
         (subaddress-p (geta address-info :subaddress))
         (public-view-key (geta address-info :public-view-key))
         (public-spend-key (geta address-info :public-spend-key))
         (transaction-public-keys (split-bytes transaction-public-key +key-length+))
         (proof (base58-decode (subseq proof header-length))))
    (verify-outbound-transaction-proof-v2 transaction-hash
                                          transaction-public-keys
                                          secret-view-key
                                          public-view-key
                                          public-spend-key
                                          subaddress-p
                                          message
                                          proof)))

(defun valid-outbound-transaction-proof-p (transaction-hash address message transaction-public-key proof secret-view-key)
  "Return T if a PROOF of outbound transaction for an ADDRESS is valid, and NIL
otherwise."
  (let ((header (subseq proof 0 (min (length +outbound-transaction-proof-v2-header+)
                                     (length proof)))))
    (cond
      ((string= header +outbound-transaction-proof-v1-header+)
       (valid-outbound-transaction-proof-v1-p transaction-hash
                                              address
                                              message
                                              transaction-public-key
                                              proof
                                              secret-view-key))
      ((string= header +outbound-transaction-proof-v2-header+)
       (valid-outbound-transaction-proof-v2-p transaction-hash
                                              address
                                              message
                                              transaction-public-key
                                              proof
                                              secret-view-key)))))
