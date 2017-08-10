;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


#+cncrypto-prefer-ffi
(defun generate-secret-key ()
  "Generate a new random secret key."
  (with-foreign-objects ((raw-data :unsigned-char (* 2 +ed25519-key-length+)))
    (cn-generate-random-bytes-not-thread-safe (* 2 +ed25519-key-length+) raw-data)
    (cn-sc-reduce raw-data)
    (let ((secret-key (make-array +ed25519-key-length+ :element-type '(unsigned-byte 8))))
      (dotimes (i +ed25519-key-length+ secret-key)
        (setf (aref secret-key i) (mem-aref raw-data :unsigned-char i))))))

#-cncrypto-prefer-ffi
(defun generate-secret-key ()
  "Generate a new random secret key."
  (random-scalar))

#+cncrypto-prefer-ffi
(defun secret-key->public-key (secret-key)
  "Compute the public key matching a SECRET-KEY."
  (check-type secret-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (with-foreign-objects ((raw-secret-key :unsigned-char +ed25519-key-length+)
                         (raw-public-key :unsigned-char +ed25519-key-length+)
                         (raw-point '(:struct cn-ge-p3)))
    (dotimes (i +ed25519-key-length+)
      (setf (mem-aref raw-secret-key :unsigned-char i) (aref secret-key i)))
    (cn-sc-reduce32 raw-secret-key)
    (cn-ge-scalarmult-base raw-point raw-secret-key)
    (cn-ge-p3-tobytes raw-public-key raw-point)
    (let ((public-key (make-array +ed25519-key-length+ :element-type '(unsigned-byte 8))))
      (dotimes (i +ed25519-key-length+ public-key)
        (setf (aref public-key i) (mem-aref raw-public-key :unsigned-char i))))))

#-cncrypto-prefer-ffi
(defun secret-key->public-key (secret-key)
  "Compute the public key matching a SECRET-KEY."
  (check-type secret-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (let* ((g ironclad::+ed25519-b+)
         (x (ironclad::ed25519-decode-int secret-key))
         (r (mod x ironclad::+ed25519-l+))
         (y (ironclad::ed25519-scalar-mult g r)))
    (ironclad::ed25519-encode-point y)))

(defun secret-spend-key->secret-view-key (secret-spend-key)
  "Derive the secret view key from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (fast-hash secret-spend-key))

(defun recover-keys (secret-spend-key)
  "Compute the public-spend-key, secret-view-key and public-view-key
from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
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
  (check-type public-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (check-type secret-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (let* ((p (ironclad::ed25519-decode-point public-key))
         (s (ironclad::ed25519-decode-int secret-key))
         (k (ge-mul8 (ironclad::ed25519-scalar-mult p s))))
    (ironclad::ed25519-encode-point k)))

(defun derivation->scalar (derivation output-index)
  "Compute a scalar that can be used for deriving an output's keys
from a key DERIVATION and an OUTPUT-INDEX."
  (check-type derivation (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (check-type output-index (integer 0))
  (let ((data (concatenate '(simple-array (unsigned-byte 8) (*))
                           derivation (write-varint output-index))))
    (hash-to-scalar data)))

(defun derive-public-key (derivation output-index public-spend-key)
  "Compute an output's public key from a key DERIVATION, an
OUTPUT-INDEX and a PUBLIC-SPEND-KEY."
  (check-type derivation (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (check-type output-index (integer 0))
  (check-type public-spend-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (let* ((g ironclad::+ed25519-b+)
         (x (ironclad::ed25519-decode-int (derivation->scalar derivation output-index)))
         (p1 (ironclad::ed25519-decode-point public-spend-key))
         (p2 (ironclad::ed25519-scalar-mult g x))
         (p3 (ironclad::ed25519-edwards-add p1 p2)))
    (ironclad::ed25519-encode-point p3)))

(defun derive-secret-spend-key (derivation output-index secret-spend-key)
  "Compute an output's secret key from a key DERIVATION, an
OUTPUT-INDEX and a SECRET-SPEND-KEY."
  (check-type derivation (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (check-type output-index (integer 0))
  (check-type secret-spend-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (let* ((x (ironclad::ed25519-decode-int (derivation->scalar derivation output-index)))
         (s (ironclad::ed25519-decode-int secret-spend-key))
         (k (mod (+ x s) ironclad::+ed25519-l+)))
    (ironclad::ed25519-encode-int k)))
