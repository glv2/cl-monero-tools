;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


;;; Public key cryptography functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +key-length+ 32))

(define-constant +g+ ironclad::+ed25519-b+ :test #'equalp)
(let ((h (hex-string->bytes "8b655970153799af2aeadc9ff1add0ea6c7251d54154cfa92c173a0dd39c1f94")))
  (define-constant +h+ (ironclad::ed25519-decode-point h) :test #'equalp))
(defconstant +q+ ironclad::+ed25519-q+)
(defconstant +l+ ironclad::+ed25519-l+)
(defconstant +i+ ironclad::+ed25519-i+)
(defconstant +a+ 486662)
(defconstant +fffb1+ 57192811444617977854858898469001663971726463542204390960804972474891788632558)
(defconstant +fffb2+ 34838897745748397871374137087405348832069628406613012804793447631241588021984)
(defconstant +fffb3+ 46719087769223307720043111813545796356806574765024592941723029582131464514662)
(defconstant +fffb4+ 11880190023474909848668974726140447524736946358411580136929581950889876492678)

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

(defun point- (p1 p2)
  "Point subtraction on Ed25519."
  (check-type p1 point)
  (check-type p2 point)
  (let ((inv-p2 (vector (- +q+ (aref p2 0))
                        (aref p2 1)
                        (aref p2 2)
                        (- +q+ (aref p2 3)))))
    (point+ p1 inv-p2)))

(defun point* (point n)
  "Scalar multiplication on Ed25519."
  (check-type point point)
  (check-type n (integer 0))
  (ironclad::ed25519-scalar-mult point n))

(defun point= (p1 p2)
  "Point equality on Ed25519."
  (check-type p1 point)
  (check-type p2 point)
  (ironclad::ed25519-point-equal p1 p2))

(defun point*8 (point)
  "Multiply a POINT by 8."
  (check-type point point)
  (let* ((p2 (point+ point point))
         (p4 (point+ p2 p2)))
    (point+ p4 p4)))

(defun reduce-scalar (data)
  "Return the byte vector representing DATA modulo +L+."
  (check-type data octet-vector)
  (integer->bytes (mod (bytes->integer data) +l+) :size +key-length+))

(defun random-scalar ()
  "Return a random number modulo +L+."
  (reduce-scalar (ironclad:random-data +key-length+)))


;;; Hash functions

(defconstant +hash-length+ 32)

(defun fast-hash (data)
  "Fast hash function (Keccak1600) for the Cryptonote protocol."
  (check-type data octet-vector)
  (subseq (keccak1600 data) 0 +hash-length+))

(defparameter *cryptonight-variant* 0)

(defun slow-hash (data &optional (variant *cryptonight-variant*))
  "Slow hash function (CryptoNight) for the Cryptonote protocol."
  (check-type data octet-vector)
  (cryptonight data variant))

(defun tree-hash (data count)
  "Tree hash function for the transactions Merkle tree."
  (check-type data octet-vector)
  (check-type count (integer 1))
  (flet ((tree-hash-count (count)
           (loop with pow = 2
                 while (< pow count)
                 do (setf pow (ash pow 1))
                 finally (return (ash pow -1)))))
    (cond
      ((= count 1)
       (subseq data 0 +hash-length+))
      ((= count 2)
       (fast-hash (subseq data 0 (* 2 +hash-length+))))
      (t
       (let* ((cnt (tree-hash-count count))
              (tmp (make-array (* cnt +hash-length+)
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))
         (replace tmp data :end2 (* (- (* 2 cnt) count) +hash-length+))
         (loop for i from (- (* 2 cnt) count) by 2
               for j from (- (* 2 cnt) count)
               while (< j cnt)
               do (setf (subseq tmp (* j +hash-length+) (* (1+ j) +hash-length+))
                        (fast-hash (subseq data (* i +hash-length+) (* (+ i 2) +hash-length+)))))
         (loop while (> cnt 2) do
           (setf cnt (ash cnt -1))
           (loop for i from 0 by 2
                 for j from 0
                 while (< j cnt)
                 do (setf (subseq tmp (* j +hash-length+) (* (1+ j) +hash-length+))
                          (fast-hash (subseq tmp (* i +hash-length+) (* (+ i 2) +hash-length+))))))
         (fast-hash (subseq tmp 0 (* 2 +hash-length+))))))))

(defun hash-to-scalar (data)
  "Make a scalar usable with the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data octet-vector)
  (reduce-scalar (fast-hash data)))

(defun hash-to-point (data)
  "Make a point on the ED25519 curve from DATA and return it
as a byte vector."
  (check-type data octet-vector)
  (let* ((u (mod (bytes->integer (fast-hash data)) +q+))
         (v (mod (* 2 u u) +q+))
         (w (mod (+ v 1) +q+))
         (x (mod (* w w) +q+))
         (y (mod (* -1 +a+ +a+ v) +q+))
         (x (mod (+ x y) +q+))
         (x3 (mod (* x x x) +q+))
         (wx3 (mod (* w x3) +q+))
         (wx7 (mod (* wx3 x3 x) +q+))
         (res-x (mod (* wx3 (ironclad:expt-mod wx7 (/ (- +q+ 5) 8) +q+)) +q+))
         (y (mod (* res-x res-x) +q+))
         (x (mod (* y x) +q+))
         (y (mod (- w x) +q+))
         (z (mod (- +a+) +q+)))
    (flet ((make-point-and-return (sign x w z)
             (unless (= (logand x 1) sign)
               (setf x (mod (- x) +q+)))
             (let* ((pz (mod (+ z w) +q+))
                    (py (mod (- z w) +q+))
                    (px (mod (* x pz) +q+))
                    (inv-pz (ironclad::ed25519-inv pz))
                    (pw (mod (* px py inv-pz) +q+))
                    (point (vector px py pz pw)))
               (return-from hash-to-point (point->bytes (point*8 point))))))
      (if (zerop y)
          (setf res-x (mod (* res-x +fffb2+) +q+))
          (progn
            (setf y (mod (+ w x) +q+))
            (if (zerop y)
                (setf res-x (mod (* res-x +fffb1+) +q+))
                (progn
                  (setf x (mod (* x +i+) +q+))
                  (setf y (mod (- w x) +q+))
                  (if (zerop y)
                      (setf res-x (mod (* res-x +fffb4+) +q+))
                      (setf res-x (mod (* res-x +fffb3+) +q+)))
                  (make-point-and-return 1 res-x w z)))))
      (setf res-x (mod (* res-x u) +q+))
      (setf z (mod (* z v) +q+))
      (make-point-and-return 0 res-x w z))))


;;; Data encryption/decryption functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +chacha-key-length+ 32)
  (defconstant +chacha-iv-length+ 8))
(defconstant +chacha-key-tail+ 140)

(defun chacha8 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data octet-vector)
  (check-type key (octet-vector #.+chacha-key-length+))
  (check-type iv (octet-vector #.+chacha-iv-length+))
  (let ((cipher (ironclad:make-cipher :chacha/8 :key key
                                                :mode :stream
                                                :initialization-vector iv))
        (ciphertext (make-array (length data) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher data ciphertext)
    ciphertext))

(defun chacha20 (data key iv)
  "Encrypt/decrypt DATA with the KEY and the initialization vector IV."
  (check-type data octet-vector)
  (check-type key (octet-vector #.+chacha-key-length+))
  (check-type iv (octet-vector #.+chacha-iv-length+))
  (let ((cipher (ironclad:make-cipher :chacha :key key
                                              :mode :stream
                                              :initialization-vector iv))
        (ciphertext (make-array (length data) :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher data ciphertext)
    ciphertext))

(defun generate-chacha-key (password)
  "Generate the encryption/decryption key matching a PASSWORD."
  (check-type password string)
  (subseq (slow-hash (utf-8-string->bytes password)) 0 +chacha-key-length+))

(defun generate-chacha-key-from-secret-keys (secret-view-key secret-spend-key)
  "Generate the encryption/decryption key matching a wallet's secret
keys."
  (check-type secret-view-key (octet-vector #.+key-length+))
  (check-type secret-spend-key (octet-vector #.+key-length+))
  (let ((data (concatenate 'octet-vector
                           secret-view-key
                           secret-spend-key
                           (vector +chacha-key-tail+))))
    (subseq (slow-hash data) 0 +chacha-key-length+)))
