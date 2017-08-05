;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun bytes->integer (bytes &key (start 0) end big-endian)
  "Convert a sequence of BYTES to an integer."
  (ironclad:octets-to-integer bytes :start start :end end :big-endian big-endian))

(defun integer->bytes (n &key buffer (start 0) size big-endian)
  "Convert an integer N to a sequence of SIZE bytes. If BUFFER is
supplied, put the bytes in it starting at index START."
  (let ((bytes (if size
                   (ironclad:integer-to-octets n :big-endian big-endian :n-bits (* 8 size))
                   (ironclad:integer-to-octets n :big-endian big-endian))))
    (if buffer
        (replace buffer bytes :start1 start)
        bytes)))

(defun string->bytes (string)
  "Convert a UTF-8 string to a sequence of bytes."
  (string-to-octets string :encoding :utf-8))

(defun hex-string->bytes (hex-string &key (start 0) end)
  "Return the part of the HEX-STRING between START and END as a byte vector."
  (let ((end (or end (length hex-string))))
    (if (oddp (- end start))
        (error "The size of the HEX-STRING must be a multiple of 2.")
        (do ((index-hs start (+ index-hs 2))
             (index-bv 0 (+ index-bv 1))
             (byte-vector (make-array (/ (- end start) 2) :element-type '(unsigned-byte 8))))
            ((>= index-hs end) byte-vector)
          (setf (aref byte-vector index-bv)
                (parse-integer hex-string
                               :start index-hs
                               :end (+ index-hs 2)
                               :radix 16))))))

(defun bytes->hex-string (byte-vector &key (start 0) end)
  "Return the part of the BYTE-VECTOR between START and END as a hex string."
  (let ((end (or end (length byte-vector))))
    (do ((index-bv start (+ index-bv 1))
         (index-hs 0 (+ index-hs 2))
         (hex-string (make-string (* (- end start) 2))))
        ((>= index-bv end) hex-string)
      (let ((s (format nil "~(~2,'0x~)" (aref byte-vector index-bv))))
        (replace hex-string s :start1 index-hs)))))

(defun geta (alist key &key (test #'eql))
  "Return the element of the ALIST specified by the KEY. GETA is
SETF-able, it is equivalent to (cdr (assoc key alist :test test))."
  (cdr (assoc key alist :test test)))

(defun (setf geta) (new-value alist key &key (test #'eql))
  (setf (cdr (assoc key alist :test test)) new-value))
