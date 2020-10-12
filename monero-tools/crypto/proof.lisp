;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +hash-key-transaction-proof-v2+ (string->bytes "TXPROOF_V2")
  :test #'equalp)

(defun generate-transaction-proof (transaction-hash recipient-public-view-key key-derivation transaction-secret-key)
  "Return a signature proving that a transaction to a recipient was
made."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type recipient-public-view-key (octet-vector #.+key-length+))
  (check-type key-derivation (octet-vector #.+key-length+))
  (check-type transaction-secret-key (octet-vector #.+key-length+))
  (let* ((a (bytes->point recipient-public-view-key))
         (s (bytes->integer transaction-secret-key))
         (k (1+ (strong-random (1- +l+))))
         (x (point* +g+ k))
         (y (point* a k))
         (c-data (hash-to-scalar (concatenate 'octet-vector
                                              transaction-hash
                                              key-derivation
                                              (point->bytes x)
                                              (point->bytes y))))
         (c (bytes->integer c-data))
         (r-data (integer->bytes (mod (- k (* c s)) +l+) :size +key-length+)))
    (concatenate 'octet-vector c-data r-data)))

(defun verify-transaction-proof (transaction-hash recipient-public-view-key key-derivation transaction-public-key proof)
  "Return T if a PROOF of transaction is valid, and NIL otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type recipient-public-view-key (octet-vector #.+key-length+))
  (check-type key-derivation (octet-vector #.+key-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type proof (octet-vector #.(* 2 +key-length+)))
  (let* ((c (bytes->integer proof :start 0 :end +key-length+))
         (r (bytes->integer proof :start +key-length+))
         (x (point+ (point* (bytes->point transaction-public-key) c)
                    (point* +g+ r)))
         (y (point+ (point* (bytes->point key-derivation) c)
                    (point* (bytes->point recipient-public-view-key) r)))
         (h (bytes->integer (hash-to-scalar (concatenate 'octet-vector
                                                         transaction-hash
                                                         key-derivation
                                                         (point->bytes x)
                                                         (point->bytes y))))))
    (= c h)))

(defun generate-transaction-proof-v2 (transaction-hash transaction-public-key recipient-public-view-key recipient-public-spend-key key-derivation transaction-secret-key)
  "Return a signature proving that a transaction to a recipient was made."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type recipient-public-view-key (octet-vector #.+key-length+))
  (check-type recipient-public-spend-key (or null (octet-vector #.+key-length+)))
  (check-type key-derivation (octet-vector #.+key-length+))
  (check-type transaction-secret-key (octet-vector #.+key-length+))
  (let* ((a (bytes->point recipient-public-view-key))
         (b (if recipient-public-spend-key
                (bytes->point recipient-public-spend-key)
                +g+))
         (s (bytes->integer transaction-secret-key))
         (k (1+ (strong-random (1- +l+))))
         (sep (fast-hash +hash-key-transaction-proof-v2+))
         (x (point* b k))
         (y (point* a k))
         (recipient-public-spend-key (or recipient-public-spend-key
                                         (make-array +key-length+
                                                     :element-type '(unsigned-byte 8)
                                                     :initial-element 0)))
         (c-data (hash-to-scalar (concatenate 'octet-vector
                                              transaction-hash
                                              key-derivation
                                              (point->bytes x)
                                              (point->bytes y)
                                              sep
                                              transaction-public-key
                                              recipient-public-view-key
                                              recipient-public-spend-key)))
         (c (bytes->integer c-data))
         (r (mod (- k (* c s)) +l+))
         (r-data (integer->bytes r :size +key-length+)))
    (concatenate 'octet-vector c-data r-data)))

(defun check-transaction-proof-v2 (transaction-hash transaction-public-key recipient-public-view-key recipient-public-spend-key key-derivation proof)
  "Return T if a PROOF of transaction is valid, and NIL otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type recipient-public-view-key (octet-vector #.+key-length+))
  (check-type recipient-public-spend-key (or null (octet-vector #.+key-length+)))
  (check-type key-derivation (octet-vector #.+key-length+))
  (check-type proof (octet-vector #.(* 2 +key-length+)))
  (let* ((p (bytes->point transaction-public-key))
         (a (bytes->point recipient-public-view-key))
         (b (if recipient-public-spend-key
                (bytes->point recipient-public-spend-key)
                +g+))
         (d (bytes->point key-derivation))
         (c (bytes->integer proof :start 0 :end +key-length+))
         (r (bytes->integer proof :start +key-length+))
         (sep (fast-hash +hash-key-transaction-proof-v2+))
         (x (point+ (point* p c) (point* b r)))
         (y (point+ (point* d c) (point* a r)))
         (recipient-public-spend-key (or recipient-public-spend-key
                                         (make-array +key-length+
                                                     :element-type '(unsigned-byte 8)
                                                     :initial-element 0)))
         (h-data (hash-to-scalar (concatenate 'octet-vector
                                              transaction-hash
                                              key-derivation
                                              (point->bytes x)
                                              (point->bytes y)
                                              sep
                                              transaction-public-key
                                              recipient-public-view-key
                                              recipient-public-spend-key)))
         (h (bytes->integer h-data)))
    (= c h)))

(defun generate-inbound-transaction-proof-v1 (transaction-hash transaction-public-key secret-view-key public-spend-key message)
  "Return a shared secret and a signature proving the existence of an
inbound transaction (with specified TRANSACTION-HASH and
TRANSACTION-PUBLIC-KEY) for a recipient (with specified
SECRET-VIEW-KEY and PUBLIC-SPEND-KEY). An arbitrary MESSAGE can be
signed along with the TRANSACTION-HASH."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (let* ((data (concatenate 'octet-vector transaction-hash message))
         (p (bytes->point transaction-public-key))
         (a (bytes->integer secret-view-key))
         (b (bytes->point public-spend-key))
         (e (point* p a))
         (e-data (point->bytes e))
         (k (1+ (strong-random (1- +l+))))
         (l1 (point* b k))
         (l2 (point* p k))
         (c-data (hash-to-scalar (concatenate 'octet-vector
                                              (fast-hash data)
                                              e-data
                                              (point->bytes l1)
                                              (point->bytes l2))))
         (c (bytes->integer c-data))
         (r (mod (- k (* c a)) +l+))
         (r-data (integer->bytes r :size +key-length+)))
    (values e-data (concatenate 'octet-vector c-data r-data))))

(defun verify-inbound-transaction-proof-v1 (transaction-hash transaction-public-key public-view-key public-spend-key message shared-secret proof &optional transaction-secret-key)
  "Return T if a PROOF of inbound transaction is valid, and NIL
otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (check-type shared-secret (octet-vector #.+key-length+))
  (check-type proof (octet-vector #.(* 2 +key-length+)))
  (let* ((data (concatenate 'octet-vector transaction-hash message))
         (p (bytes->point transaction-public-key))
         (a (bytes->point public-view-key))
         (b (bytes->point public-spend-key))
         (e (bytes->point shared-secret))
         (c (bytes->integer proof :start 0 :end +key-length+))
         (r (bytes->integer proof :start +key-length+))
         (l1 (point+ (point* a c) (point* b r)))
         (l2 (point+ (point* e c) (point* p r)))
         (h (bytes->integer (hash-to-scalar (concatenate 'octet-vector
                                                         (fast-hash data)
                                                         shared-secret
                                                         (point->bytes l1)
                                                         (point->bytes l2))))))
    (if (null transaction-secret-key)
        (= c h)
        (let* ((s (bytes->point transaction-secret-key))
               (f (point* a s)))
          (and (point= e f) (= c h))))))

(defun generate-inbound-transaction-proof-v2 (transaction-hash transaction-public-keys secret-view-key public-view-key public-spend-key subaddress-p message)
  "Return shared secrets and a signatures proving the existence of an inbound
transaction (with specified TRANSACTION-HASH and TRANSACTION-PUBLIC-KEY) for
a recipient (with specified SECRET-VIEW-KEY, PUBLIC-VIEW-KEY and
PUBLIC-SPEND-KEY). An arbitrary MESSAGE can be signed along with the
TRANSACTION-HASH."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-keys list)
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (let* ((data (fast-hash (concatenate 'octet-vector transaction-hash message)))
         (a (bytes->integer secret-view-key)))
    (flet ((compute-proof (public-key)
             (let* ((p (bytes->point public-key))
                    (shared-secret (point->bytes (point* p a)))
                    (proof (generate-transaction-proof-v2 data
                                                          public-view-key
                                                          public-key
                                                          (if subaddress-p
                                                              public-spend-key
                                                              nil)
                                                          shared-secret
                                                          secret-view-key)))
               (list shared-secret proof))))
      (join-bytes (mapcan #'compute-proof transaction-public-keys)))))

(defun verify-inbound-transaction-proof-v2 (transaction-hash transaction-public-keys secret-view-key public-view-key public-spend-key subaddress-p message proof)
  "Return T if a PROOF of inbound transaction is valid, and NIL otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-keys list)
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (check-type proof octet-vector)
  (let* ((data (fast-hash (concatenate 'octet-vector transaction-hash message)))
         (shared-secrets (iter (for i from 0 below (length proof) by (* 3 +key-length+))
                               (collect (subseq proof i (+ i +key-length+)))))
         (proofs (iter (for i from +key-length+ below (length proof) by (* 3 +key-length+))
                       (collect (subseq proof i (+ i (* 2 +key-length+)))))))
    (flet ((check-proof (public-key shared-secret proof)
             (check-transaction-proof-v2 data
                                         public-view-key
                                         public-key
                                         (if subaddress-p
                                             public-spend-key
                                             nil)
                                         shared-secret
                                         proof)))
      (some #'check-proof transaction-public-keys shared-secrets proofs))))

(defun generate-outbound-transaction-proof-v1 (transaction-hash transaction-secret-key public-view-key public-spend-key message)
  "Return a shared secret and a signature proving the existence of an
outbound transaction (with specified TRANSACTION-HASH and
TRANSACTION-SECRET-KEY) for a recipient (with specified
PUBLIC-VIEW-KEY and PUBLIC-SPEND-KEY). An arbitrary MESSAGE can be
signed along with the TRANSACTION-HASH."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-secret-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (let* ((data (concatenate 'octet-vector transaction-hash message))
         (s (bytes->integer transaction-secret-key))
         (a (bytes->point public-view-key))
         (b (bytes->point public-spend-key))
         (e (point* a s))
         (e-data (point->bytes e)) ; TODO: check validity of shared secret using secret-view-key
         (k (1+ (strong-random (1- +l+))))
         (l1 (point* b k))
         (l2 (point* a k))
         (c-data (hash-to-scalar (concatenate 'octet-vector
                                              (fast-hash data)
                                              e-data
                                              (point->bytes l1)
                                              (point->bytes l2))))
         (c (bytes->integer c-data))
         (r (mod (- k (* c s)) +l+))
         (r-data (integer->bytes r :size +key-length+)))
    (values e-data (concatenate 'octet-vector c-data r-data))))

(defun verify-outbound-transaction-proof-v1 (transaction-hash transaction-public-key public-view-key public-spend-key message shared-secret proof &optional secret-view-key)
  "Return T if a PROOF of outbound transaction is valid, and NIL
otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (check-type shared-secret (octet-vector #.+key-length+))
  (check-type proof (octet-vector #.(* 2 +key-length+)))
  (let* ((data (concatenate 'octet-vector transaction-hash message))
         (p (bytes->point transaction-public-key))
         (a (bytes->point public-view-key))
         (b (bytes->point public-spend-key))
         (e (bytes->point shared-secret)) ; TODO: check validity of shared secret
         (c (bytes->integer proof :start 0 :end +key-length+))
         (r (bytes->integer proof :start +key-length+))
         (l1 (point+ (point* p c) (point* b r)))
         (l2 (point+ (point* e c) (point* a r)))
         (h (bytes->integer (hash-to-scalar (concatenate 'octet-vector
                                                         (fast-hash data)
                                                         shared-secret
                                                         (point->bytes l1)
                                                         (point->bytes l2))))))
    (if (null secret-view-key)
        (= c h)
        (let* ((s (bytes->integer secret-view-key))
               (f (point* p s)))
          (and (point= e f) (= c h))))))

(defun generate-outbound-transaction-proof-v2 (transaction-hash transaction-secret-keys public-view-key public-spend-key subaddress-p message)
  "Return shared secrets and signatures proving the existence of an outbound
transaction (with specified TRANSACTION-HASH and TRANSACTION-SECRET-KEYS) for
a recipient (with specified PUBLIC-VIEW-KEY, PUBLIC-SPEND-KEY and
SUBADDRESS-P). An arbitrary MESSAGE can be signed along with the
TRANSACTION-HASH."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-secret-keys list)
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (let* ((data (fast-hash (concatenate 'octet-vector transaction-hash message)))
         (a (bytes->point public-view-key))
         (b (bytes->point public-spend-key)))
    (flet ((compute-proof (secret-key)
             (let* ((s (bytes->integer secret-key))
                    (shared-secret (point->bytes (point* a s)))
                    (p (if subaddress-p b +g+))
                    (public-key (point->bytes (point* p s)))
                    (proof (generate-transaction-proof-v2 data
                                                          public-key
                                                          public-view-key
                                                          (if subaddress-p
                                                              public-spend-key
                                                              nil)
                                                          shared-secret
                                                          secret-key)))
               (list shared-secret proof))))
      (join-bytes (mapcan #'compute-proof transaction-secret-keys)))))

(defun verify-outbound-transaction-proof-v2 (transaction-hash transaction-public-keys secret-view-key public-view-key public-spend-key subaddress-p message proof)
  "Return T if a PROOF of outbound transaction is valid, and NIL otherwise."
  (check-type transaction-hash (octet-vector #.+hash-length+))
  (check-type transaction-public-keys list)
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type public-view-key (octet-vector #.+key-length+))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (check-type message octet-vector)
  (check-type proof octet-vector)
  (let* ((data (fast-hash (concatenate 'octet-vector transaction-hash message)))
         (shared-secrets (iter
                           (for i from 0 below (length proof) by (* 3 +key-length+))
                           (collect (subseq proof i (+ i +key-length+)))))
         (proofs (iter
                   (for i from +key-length+ below (length proof) by (* 3 +key-length+))
                   (collect (subseq proof i (+ i (* 2 +key-length+)))))))
    (flet ((check-proof (public-key shared-secret proof)
             (check-transaction-proof-v2 data
                                         public-key
                                         public-view-key
                                         (if subaddress-p
                                             public-spend-key
                                             nil)
                                         shared-secret
                                         proof)))
      (some #'check-proof transaction-public-keys shared-secrets proofs))))
