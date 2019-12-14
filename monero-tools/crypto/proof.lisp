;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


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

(defun generate-inbound-transaction-proof (transaction-hash transaction-public-key secret-view-key public-spend-key message)
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

(defun verify-inbound-transaction-proof (transaction-hash transaction-public-key public-view-key public-spend-key message shared-secret proof &optional transaction-secret-key)
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

(defun generate-outbound-transaction-proof (transaction-hash transaction-secret-key public-view-key public-spend-key message)
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

(defun verify-outbound-transaction-proof (transaction-hash transaction-public-key public-view-key public-spend-key message shared-secret proof &optional secret-view-key)
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
        (let* ((s (bytes->point secret-view-key))
               (f (point* p s)))
          (and (point= e f) (= c h))))))
