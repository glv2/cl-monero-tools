;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +encrypted-payment-id-tail+ 141)
(define-constant +payment-proof-header+ "ProofV1" :test #'string=)

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
         (key (derive-public-key derivation output-index public-spend-key)))
    (equalp key output-key)))

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
        (valid-transaction-proof-p transaction-hash
                                   recipient-public-view-key
                                   key-derivation
                                   transaction-public-key
                                   proof-data)))))
