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
  (sc-reduce (ironclad:random-data 32)))

#+cncrypto-prefer-ffi
(defun secret-key->public-key (secret-key)
  "Compute the public key matching a SECRET-KEY."
  (check-type secret-key (simple-array (unsigned-byte 8) (*)))
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
  (check-type secret-key (simple-array (unsigned-byte 8) (*)))
  (let* ((g ironclad::+ed25519-b+)
         (x (ironclad::ed25519-decode-int secret-key))
         (r (mod x ironclad::+ed25519-l+))
         (y (ironclad::ed25519-scalar-mult g r)))
    (ironclad::ed25519-encode-point y)))

(defun secret-spend-key->secret-view-key (secret-spend-key)
  "Derive the secret view key from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (simple-array (unsigned-byte 8) (*)))
  (fast-hash secret-spend-key))

(defun recover-keys (secret-spend-key)
  "Compute the public-spend-key, secret-view-key and public-view-key
from the SECRET-SPEND-KEY."
  (check-type secret-spend-key (simple-array (unsigned-byte 8) (*)))
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
