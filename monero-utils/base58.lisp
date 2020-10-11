;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-utils)


(define-constant +base58-alphabet+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  :test #'string=)
(defconstant +base58-alphabet-size+ 58)
(define-constant +base58-encoded-block-sizes+ #(0 2 3 5 6 7 9 10 11) :test #'equalp)
(defconstant +base58-full-block-size+ 8)
(defconstant +base58-full-encoded-block-size+ 11)
(defconstant +base58-checksum-size+ 4)

(defun base58-encoded-length (n)
  "Return the length of the base58 encoding of N bytes."
  (multiple-value-bind (full-block-count remaining-bytes)
      (floor n +base58-full-block-size+)
    (+ (* full-block-count +base58-full-encoded-block-size+)
       (aref +base58-encoded-block-sizes+ remaining-bytes))))

(defun base58-decoded-length (n)
  "Return the number of bytes encoded in a base58 string of
N characters."
  (multiple-value-bind (full-encoded-block-count remaining-characters)
      (floor n +base58-full-encoded-block-size+)
    (+ (* full-encoded-block-count +base58-full-block-size+)
       (or (position remaining-characters +base58-encoded-block-sizes+)
           (error "Invalid size.")))))

(defun base58-encode (data)
  "Return the base58 encoding of the DATA byte sequence."
  (multiple-value-bind (full-block-count last-block-size)
      (floor (length data) +base58-full-block-size+)
    (flet ((encode-block (data start size)
             (do* ((encoded-size (aref +base58-encoded-block-sizes+ size))
                   (encoded-string (make-string encoded-size
                                                :initial-element (aref +base58-alphabet+ 0)))
                   (n (bytes->integer data :start start :end (+ start size) :big-endian t))
                   (r 0)
                   (i (1- encoded-size) (1- i)))
                  ((zerop n) encoded-string)
               (setf (values n r) (floor n +base58-alphabet-size+))
               (setf (aref encoded-string i) (aref +base58-alphabet+ r)))))
      (with-output-to-string (output-stream)
        (dotimes (i full-block-count)
          (write-string (encode-block data
                                      (* i +base58-full-block-size+)
                                      +base58-full-block-size+)
                        output-stream))
        (when (plusp last-block-size)
          (write-string (encode-block data
                                      (* full-block-count +base58-full-block-size+)
                                      last-block-size)
                        output-stream))))))

(defun base58-decode (string)
  "Return the bytes encoded in a base58 STRING."
  (multiple-value-bind (full-block-count last-block-size)
      (floor (length string) +base58-full-encoded-block-size+)
    (flet ((decode-block (data start size)
             (do* ((decoded-size (position size +base58-encoded-block-sizes+))
                   (n 0)
                   (order 1 (* order +base58-alphabet-size+))
                   (i (1- size) (1- i)))
                  ((minusp i)
                   (if (and (< decoded-size +base58-full-block-size+)
                            (<= (ash 1 (* 8 decoded-size)) n))
                       (error "Overflow.")
                       (integer->bytes n :size decoded-size :big-endian t)))
               (let ((digit (position (aref data (+ start i)) +base58-alphabet+)))
                 (unless digit
                   (error "Invalid symbol."))
                 (let ((tmp (+ n (* order digit))))
                   (when (or (< tmp n) (plusp (ash tmp -64)))
                     (error "Overflow."))
                   (setf n tmp))))))
      (let ((last-block-decoded-size (position last-block-size +base58-encoded-block-sizes+)))
        (unless last-block-decoded-size
          (error "Invalid size."))
        (let ((data (make-array (+ (* full-block-count +base58-full-block-size+)
                                   last-block-decoded-size)
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
          (dotimes (i full-block-count)
            (replace data
                     (decode-block string
                                   (* i +base58-full-encoded-block-size+) 
                                   +base58-full-encoded-block-size+)
                     :start1 (* i +base58-full-block-size+)))
          (when (plusp last-block-size)
            (replace data
                     (decode-block string
                                   (* full-block-count +base58-full-encoded-block-size+)
                                   last-block-size)
                     :start1 (* full-block-count +base58-full-block-size+)))
          data)))))
