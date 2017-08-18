;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


#+cncrypto-prefer-ffi
(defun generate-secret-key ()
  "Generate a new random secret key."
  (with-foreign-objects ((raw-data :unsigned-char (* 2 +key-length+)))
    (cn-generate-random-bytes-not-thread-safe (* 2 +key-length+) raw-data)
    (cn-sc-reduce raw-data)
    (let ((secret-key (make-array +key-length+ :element-type '(unsigned-byte 8))))
      (dotimes (i +key-length+ secret-key)
        (setf (aref secret-key i) (mem-aref raw-data :unsigned-char i))))))

#-cncrypto-prefer-ffi
(defun generate-secret-key ()
  "Generate a new random secret key."
  (random-scalar))

#+cncrypto-prefer-ffi
(defun secret-key->public-key (secret-key)
  "Compute the public key matching a SECRET-KEY."
  (check-type secret-key (octet-vector #.+key-length+))
  (with-foreign-objects ((raw-secret-key :unsigned-char +key-length+)
                         (raw-public-key :unsigned-char +key-length+)
                         (raw-point '(:struct cn-ge-p3)))
    (dotimes (i +key-length+)
      (setf (mem-aref raw-secret-key :unsigned-char i) (aref secret-key i)))
    (cn-sc-reduce32 raw-secret-key)
    (cn-ge-scalarmult-base raw-point raw-secret-key)
    (cn-ge-p3-tobytes raw-public-key raw-point)
    (let ((public-key (make-array +key-length+ :element-type '(unsigned-byte 8))))
      (dotimes (i +key-length+ public-key)
        (setf (aref public-key i) (mem-aref raw-public-key :unsigned-char i))))))

#-cncrypto-prefer-ffi
(defun secret-key->public-key (secret-key)
  "Compute the public key matching a SECRET-KEY."
  (check-type secret-key (octet-vector #.+key-length+))
  (let* ((x (bytes->integer secret-key))
         (r (mod x +l+))
         (y (point* +g+ r)))
    (point->bytes y)))

(defun secret-spend-key->secret-view-key (secret-spend-key)
  "Derive the secret view key from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (octet-vector #.+key-length+))
  (fast-hash secret-spend-key))

(defun recover-keys (secret-spend-key)
  "Compute the public-spend-key, secret-view-key and public-view-key
from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (octet-vector #.+key-length+))
  (let* ((secret-view-key (secret-spend-key->secret-view-key secret-spend-key))
         (public-spend-key (secret-key->public-key secret-spend-key))
         (public-view-key (secret-key->public-key secret-view-key)))
    (list (cons :public-spend-key public-spend-key)
          (cons :public-view-key public-view-key)
          (cons :secret-spend-key secret-spend-key)
          (cons :secret-view-key secret-view-key))))

(defun generate-keys ()
  "Generate a new set of keys."
  (let ((secret-spend-key (generate-secret-key)))
    (recover-keys secret-spend-key)))

(defun derive-key (public-key secret-key)
  "Compute a shared secret from a user's PUBLIC-KEY and your SECRET-KEY."
  (check-type public-key (octet-vector #.+key-length+))
  (check-type secret-key (octet-vector #.+key-length+))
  (let* ((p (bytes->point public-key))
         (s (bytes->integer secret-key))
         (k (point*8 (point* p s))))
    (point->bytes k)))

(defun derivation->scalar (derivation output-index)
  "Compute a scalar that can be used for deriving an output's keys
from a key DERIVATION and an OUTPUT-INDEX."
  (check-type derivation (octet-vector #.+key-length+))
  (check-type output-index (integer 0))
  (let ((data (concatenate 'octet-vector derivation (write-varint output-index))))
    (hash-to-scalar data)))

(defun derive-public-key (derivation output-index public-spend-key)
  "Compute an output's public key from a key DERIVATION, an
OUTPUT-INDEX and a PUBLIC-SPEND-KEY."
  (check-type derivation (octet-vector #.+key-length+))
  (check-type output-index (integer 0))
  (check-type public-spend-key (octet-vector #.+key-length+))
  (let ((x (bytes->integer (derivation->scalar derivation output-index)))
        (p (bytes->point public-spend-key)))
    (point->bytes (point+ p (point* +g+ x)))))

(defun derive-secret-key (derivation output-index secret-spend-key)
  "Compute an output's secret key from a key DERIVATION, an
OUTPUT-INDEX and a SECRET-SPEND-KEY."
  (check-type derivation (octet-vector #.+key-length+))
  (check-type output-index (integer 0))
  (check-type secret-spend-key (octet-vector #.+key-length+))
  (let ((x (bytes->integer (derivation->scalar derivation output-index)))
        (s (bytes->integer secret-spend-key)))
    (integer->bytes (mod (+ x s) +l+) :size +key-length+)))
