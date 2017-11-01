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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +key-length+ 32))

(define-constant +g+ ironclad::+ed25519-b+ :test #'equalp)
(let ((h (coerce '(#x8b #x65 #x59 #x70 #x15 #x37 #x99 #xaf
                   #x2a #xea #xdc #x9f #xf1 #xad #xd0 #xea
                   #x6c #x72 #x51 #xd5 #x41 #x54 #xcf #xa9
                   #x2c #x17 #x3a #x0d #xd3 #x9c #x1f #x94)
                 'octet-vector)))
  (define-constant +h+ (ironclad::ed25519-decode-point h) :test #'equalp))
(defconstant +l+ ironclad::+ed25519-l+)

(deftype point ()
  'ironclad::ed25519-point)

(defun point->bytes (point)
  "Convert an Ed25519 POINT to a sequence of bytes."
  (check-type point point)
  (ironclad::ed25519-encode-point point))

(defun bytes->point (bytes)
  "Convert a sequence of BYTES to an Ed25519 point."
  (ironclad::ed25519-decode-point bytes))

(defun point+ (p1 p2)
  "Point addition on Ed25519."
  (check-type p1 point)
  (check-type p2 point)
  (ironclad::ed25519-edwards-add p1 p2))

(defun point* (point n)
  "Scalar multiplication on Ed25519."
  (check-type point point)
  (check-type n (integer 0))
  (ironclad::ed25519-scalar-mult point n))

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
(defun reduce-scalar (data)
  "Return the byte vector representing DATA modulo +L+."
  (check-type data octet-vector)
  (let ((length (length data)))
    (with-foreign-object (raw-data :unsigned-char length)
      (lisp-array->c-array data raw-data)
      (cn-sc-reduce raw-data)
      (c-array->lisp-array raw-data +key-length+))))

#-cncrypto-prefer-ffi
(defun reduce-scalar (data)
  "Return the byte vector representing DATA modulo +L+."
  (check-type data octet-vector)
  (integer->bytes (mod (bytes->integer data) +l+) :size +key-length+))

(defcfun ("ge_scalarmult_base" cn-ge-scalarmult-base) :void
  (h :pointer)
  (a :pointer))

(defcfun ("ge_mul8" cn-ge-mul8) :void
  (r :pointer)
  (p :pointer))

(defun point*8 (point)
  "Multiply a POINT by 8."
  (let* ((p2 (point+ point point))
         (p4 (point+ p2 p2)))
    (point+ p4 p4)))

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

(defun random-scalar ()
  (reduce-scalar (ironclad:random-data +key-length+)))


;;; Hash functions

(defconstant +hash-length+ 32)

(defcfun ("cn_fast_hash" cn-fast-hash) :void
  (data :pointer)
  (length :unsigned-int)
  (hash :pointer))

(defun fast-hash (data)
  "Fast hash function (Keccak1600) for the Cryptonote protocol."
  (check-type data octet-vector)
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-hash :unsigned-char +hash-length+))
      (lisp-array->c-array data raw-data)
      (cn-fast-hash raw-data length raw-hash)
      (c-array->lisp-array raw-hash +hash-length+))))

(defcfun ("cn_slow_hash" cn-slow-hash) :void
  (data :pointer)
  (length :unsigned-int)
  (hash :pointer))

(defun slow-hash (data)
  "Slow hash function (CryptoNight) for the Cryptonote protocol."
  (check-type data octet-vector)
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-hash :unsigned-char +hash-length+))
      (lisp-array->c-array data raw-data)
      (cn-slow-hash raw-data length raw-hash)
      (c-array->lisp-array raw-hash +hash-length+))))

(defcfun ("tree_hash" cn-tree-hash) :void
  (hashes :pointer)
  (count :unsigned-int)
  (root-hash :pointer))

(defun tree-hash (data count)
  "Tree hash function for the transactions Merkle tree."
  (check-type data octet-vector)
  (check-type count (integer 0))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-hash :unsigned-char +hash-length+))
      (lisp-array->c-array data raw-data)
      (cn-tree-hash raw-data count raw-hash)
      (c-array->lisp-array raw-hash +hash-length+))))

#+cncrypto-prefer-ffi
(defun hash-to-scalar (data)
  "Make a scalar usable with the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data octet-vector)
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-res :unsigned-char +hash-length+))
      (lisp-array->c-array data raw-data)
      (cn-fast-hash raw-data length raw-res)
      (cn-sc-reduce32 raw-res)
      (c-array->lisp-array raw-res +key-length+))))

#-cncrypto-prefer-ffi
(defun hash-to-scalar (data)
  "Make a scalar usable with the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data octet-vector)
  (reduce-scalar (fast-hash data)))

(defun cn-hash-to-ec (raw-data raw-res)
  (with-foreign-objects ((raw-hash :unsigned-char +hash-length+)
                         (raw-point1 '(:struct cn-ge-p2))
                         (raw-point2 '(:struct cn-ge-p1p1)))
    (cn-fast-hash raw-data +key-length+ raw-hash)
    (cn-ge-fromfe-frombytes-vartime raw-point1 raw-hash)
    (cn-ge-mul8 raw-point2 raw-point1)
    (cn-ge-p1p1-to-p3 raw-res raw-point2)))

(defun hash-to-point (data)
  "Make a point on the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data octet-vector)
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-point '(:struct cn-ge-p3))
                           (raw-res :unsigned-char +key-length+))
      (lisp-array->c-array data raw-data)
      (cn-hash-to-ec raw-data raw-point)
      (cn-ge-p3-tobytes raw-res raw-point)
      (c-array->lisp-array raw-res +key-length+))))


;;; Data encryption/decryption functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +chacha8-key-length+ 32)
  (defconstant +chacha8-iv-length+ 8))
(defconstant +chacha8-key-tail+ 140)

(defcfun ("chacha8" cn-chacha8) :void
  (data :pointer)
  (length :unsigned-int)
  (key :pointer)
  (iv :pointer)
  (cipher :pointer))

#+cncrypto-prefer-ffi
(defun chacha8 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data octet-vector)
  (check-type key (octet-vector #.+chacha8-key-length+))
  (check-type iv (octet-vector #.+chacha8-iv-length+))
  (let ((length (length data)))
    (with-foreign-objects ((raw-data :unsigned-char length)
                           (raw-key :unsigned-char +chacha8-key-length+)
                           (raw-iv :unsigned-char +chacha8-iv-length+)
                           (raw-cipher :unsigned-char length))
      (lisp-array->c-array data raw-data)
      (lisp-array->c-array key raw-key)
      (lisp-array->c-array iv raw-iv)
      (cn-chacha8 raw-data length raw-key raw-iv raw-cipher)
      (c-array->lisp-array raw-cipher length))))

#-cncrypto-prefer-ffi
(defun chacha8 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data octet-vector)
  (check-type key (octet-vector #.+chacha8-key-length+))
  (check-type iv (octet-vector #.+chacha8-iv-length+))
  (let ((cipher (ironclad:make-cipher :chacha/8 :key key
                                                :mode :stream
                                                :initialization-vector iv))
        (ciphertext (make-array (length data) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher data ciphertext)
    ciphertext))

(defun generate-chacha8-key (password)
  "Generate the encryption/decryption key matching a PASSWORD."
  (check-type password string)
  (subseq (slow-hash (utf-8-string->bytes password)) 0 +chacha8-key-length+))

(defun generate-chacha8-key-from-secret-keys (secret-view-key secret-spend-key)
  "Generate the encryption/decryption key matching a wallet's secret
keys."
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type secret-spend-key (octet-vector #.+key-length+))
  (let ((data (concatenate 'octet-vector
                           secret-view-key
                           secret-spend-key
                           (vector +chacha8-key-tail+))))
    (subseq (slow-hash data) 0 +chacha8-key-length+)))
