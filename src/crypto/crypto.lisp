;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (pushnew :cncrypto-prefer-ffi *features*))

(pushnew (asdf:system-relative-pathname 'monero-tools "lib/")
         *foreign-library-directories*
         :test #'equal)

(define-foreign-library cncrypto
  (t (:default #+(or arm ppc x86) "libcncrypto32"
               #+(or arm64 ppc64 x86-64) "libcncrypto64"
               #-(or arm ppx x86 arm64 ppc64 x86-64) "libcncrypto")))

(use-foreign-library cncrypto)


;;; Public key cryptography functions

(defconstant +ed25519-key-length+ 32)

(defcstruct cn-ge-p2
  (x :uint32 :count 10)
  (y :uint32 :count 10)
  (z :uint32 :count 10))

(defcstruct cn-ge-p3
  (x :uint32 :count 10)
  (y :uint32 :count 10)
  (z :uint32 :count 10)
  (t :uint32 :count 10))

(defcstruct cn-ge-p1p1
  (x :uint32 :count 10)
  (y :uint32 :count 10)
  (z :uint32 :count 10)
  (t :uint32 :count 10))

(defcfun ("sc_reduce" cn-sc-reduce) :void
  (s :pointer))

(defcfun ("sc_reduce32" cn-sc-reduce32) :void
  (s :pointer))

#+cncrypto-prefer-ffi
(defun sc-reduce (data)
  "Return the byte vector representing DATA modulo +ED25519-L+."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (cn-sc-reduce raw-data)
      (let ((res (make-array +ed25519-key-length+ :element-type '(unsigned-byte 8))))
        (dotimes (i +ed25519-key-length+ res)
          (setf (aref res i) (mem-aref raw-data :unsigned-char i)))))))

#-cncrypto-prefer-ffi
(defun sc-reduce (data)
  "Return the byte vector representing DATA modulo +ED25519-L+."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let* ((n (ironclad::ed25519-decode-int data))
         (r (mod n ironclad::+ed25519-l+)))
    (ironclad::ed25519-encode-int r)))

(defcfun ("ge_scalarmult_base" cn-ge-scalarmult-base) :void
  (h :pointer)
  (a :pointer))

(defcfun ("ge_mul8" cn-ge-mul8) :void
  (r :pointer)
  (p :pointer))

(defcfun ("ge_p1p1_to_p3" cn-ge-p1p1-to-p3) :void
  (r :pointer)
  (p :pointer))

(defcfun ("ge_p3_tobytes" cn-ge-p3-tobytes) :void
  (s :pointer)
  (h :pointer))

(defcfun ("ge_fromfe_frombytes_vartime" cn-ge-fromfe-frombytes-vartime) :void
  (r :pointer)
  (s :pointer))

(defcfun ("generate_random_bytes_not_thread_safe" cn-generate-random-bytes-not-thread-safe) :void
  (n :unsigned-int)
  (result :pointer))


;;; Hash functions

(defconstant +hash-length+ 32)

(defcfun ("cn_fast_hash" cn-fast-hash) :void
  (data :pointer)
  (length :unsigned-int)
  (hash :pointer))

(defun fast-hash (data)
  "Fast hash function (Keccak1600) for the Cryptonote protocol."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-hash :unsigned-char +hash-length+))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (cn-fast-hash raw-data length raw-hash)
      (let ((hash (make-array +hash-length+ :element-type '(unsigned-byte 8))))
        (dotimes (i +hash-length+ hash)
          (setf (aref hash i) (mem-aref raw-hash :unsigned-char i)))))))

(defcfun ("cn_slow_hash" cn-slow-hash) :void
  (data :pointer)
  (length :unsigned-int)
  (hash :pointer))

(defun slow-hash (data)
  "Slow hash function (CryptoNight) for the Cryptonote protocol."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-hash :unsigned-char +hash-length+))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (cn-slow-hash raw-data length raw-hash)
      (let ((hash (make-array +hash-length+ :element-type '(unsigned-byte 8))))
        (dotimes (i +hash-length+ hash)
          (setf (aref hash i) (mem-aref raw-hash :unsigned-char i)))))))

#+cncrypto-prefer-ffi
(defun hash-to-scalar (data)
  "Make a scalar usable with the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-res :unsigned-char +hash-length+))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (cn-fast-hash raw-data length raw-res)
      (cn-sc-reduce32 raw-res)
      (let ((res (make-array +ed25519-key-length+ :element-type '(unsigned-byte 8))))
        (dotimes (i +ed25519-key-length+ res)
          (setf (aref res i) (mem-aref raw-res :unsigned-char i)))))))

#-cncrypto-prefer-ffi
(defun hash-to-scalar (data)
  "Make a scalar usable with the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (sc-reduce (fast-hash data)))

(defun cn-hash-to-ec (raw-data raw-res)
  (with-foreign-objects ((raw-hash :unsigned-char +hash-length+)
                         (raw-point1 '(:struct cn-ge-p2))
                         (raw-point2 '(:struct cn-ge-p1p1)))
    (cn-fast-hash raw-data +ed25519-key-length+ raw-hash)
    (cn-ge-fromfe-frombytes-vartime raw-point1 raw-hash)
    (cn-ge-mul8 raw-point2 raw-point1)
    (cn-ge-p1p1-to-p3 raw-res raw-point2)))

(defun hash-to-point (data)
  "Make a point on the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-point '(:struct cn-ge-p3))
                           (raw-res :unsigned-char +ed25519-key-length+))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (cn-hash-to-ec raw-data raw-point)
      (cn-ge-p3-tobytes raw-res raw-point)
      (let ((res (make-array +ed25519-key-length+ :element-type '(unsigned-byte 8))))
        (dotimes (i +ed25519-key-length+ res)
          (setf (aref res i) (mem-aref raw-res :unsigned-char i)))))))


;;; Data encryption/decryption functions

(defconstant +chacha8-key-length+ 32)
(defconstant +chacha8-iv-length+ 8)

(defcfun ("chacha8" cn-chacha8) :void
  (data :pointer)
  (length :unsigned-int)
  (key :pointer)
  (iv :pointer)
  (cipher :pointer))

#+cncrypto-prefer-ffi
(defun chacha8 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (check-type key (simple-array (unsigned-byte 8) (*)))
  (check-type iv (simple-array (unsigned-byte 8) (*)))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-key :unsigned-char +chacha8-key-length+)
                           (raw-iv :unsigned-char +chacha8-iv-length+)
                           (raw-cipher :unsigned-char length))
      (dotimes (i length)
        (setf (mem-aref raw-data :unsigned-char i) (aref data i)))
      (dotimes (i +chacha8-key-length+)
        (setf (mem-aref raw-key :unsigned-char i) (aref key i)))
      (dotimes (i +chacha8-iv-length+)
        (setf (mem-aref raw-iv :unsigned-char i) (aref iv i)))
      (cn-chacha8 raw-data length raw-key raw-iv raw-cipher)
      (let ((ciphertext (make-array length :element-type '(unsigned-byte 8))))
        (dotimes (i length ciphertext)
          (setf (aref ciphertext i) (mem-aref raw-cipher :unsigned-char i)))))))

#-cncrypto-prefer-ffi
(defun chacha8 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (check-type key (simple-array (unsigned-byte 8) (*)))
  (check-type iv (simple-array (unsigned-byte 8) (*)))
  (let ((cipher (ironclad:make-cipher :chacha/8 :key key
                                                :mode :stream
                                                :initialization-vector iv))
        (ciphertext (make-array (length data) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher data ciphertext)
    ciphertext))

(defun generate-chacha8-key (password)
  "Generate the decryption key matching a PASSWORD."
  (check-type password string)
  (subseq (slow-hash (string->bytes password)) 0 +chacha8-key-length+))
