;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(deftype octet-vector (&optional size)
  (if size
      `(simple-array (unsigned-byte 8) (,size))
      '(simple-array (unsigned-byte 8) (*))))

(defun read-varint (bytes &key (start 0))
  "Read the variable size integer encoded in BYTES."
  (let ((n 0)
        (size 0))
    (do ((l (length bytes))
         (i start (1+ i))
         (j 0 (+ j 7)))
        ((= i l))
      (let ((b (aref bytes i)))
        (incf n (ash (logand b #x7f) j))
        (incf size)
        (when (zerop (logand b #x80))
          (return))))
    (values n size)))

(defun write-varint (n)
  "Return the variable size integer encoding of an integer N."
  (let* ((size (max 1 (ceiling (integer-length n) 7)))
         (bytes (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size bytes)
      (setf (aref bytes i) (logior (logand n #x7f) (if (< i (- size 1)) #x80 0)))
      (setf n (ash n -7)))))

(defun bytes->integer (bytes &key (start 0) end big-endian varint)
  "Convert a sequence of BYTES to an integer."
  (if varint
      (read-varint bytes :start start)
      (let ((end (or end (length bytes)))
            (n (ironclad:octets-to-integer bytes :start start :end end :big-endian big-endian)))
        (values n (- end start)))))

(defun integer->bytes (n &key buffer (start 0) size big-endian varint)
  "Convert an integer N to a sequence of SIZE bytes. If BUFFER is
supplied, put the bytes in it starting at index START."
  (let ((bytes (if varint
                   (write-varint n)
                   (if size
                       (ironclad:integer-to-octets n :big-endian big-endian :n-bits (* 8 size))
                       (ironclad:integer-to-octets n :big-endian big-endian)))))
    (if buffer
        (replace buffer bytes :start1 start)
        bytes)))

(defun string->bytes (string)
  "Convert a STRING to a sequence of bytes."
  (map 'octet-vector #'char-code string))

(defun bytes->string (byte-vector &key (start 0) end)
  "Convert a sequence of bytes to a string."
  (let ((data (subseq byte-vector start end)))
    (map 'string #'code-char data)))

(defun utf-8-string->bytes (string)
  "Convert a UTF-8 STRING to a sequence of bytes."
  (string-to-octets string :encoding :utf-8))

(defun bytes->utf-8-string (byte-vector &key (start 0) end)
  "Convert a sequence of bytes to a UTF-8 string."
  (octets-to-string byte-vector :start start :end end :encoding :utf-8))

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

(defun read-float (string)
  (labels ((parse-digit (character)
             (- (char-code character) (char-code #\0)))

           (parse (numerator denominator point index length)
             (if (= index length)
                 (values (/ numerator denominator) length)
                 (let ((c (char string index)))
                   (cond ((char<= #\0 c #\9)
                          (parse (+ (* numerator 10) (parse-digit c))
                                 (if point (* denominator 10) denominator)
                                 point
                                 (1+ index)
                                 length))

                         ((and (not point)
                               (or (char= c #\.) (char= c #\,)))
                          (parse numerator denominator t (1+ index) length))

                         (t
                          (parse numerator denominator point index index)))))))
    (let ((length (length string)))
      (if (zerop length)
          0
          (case (char string 0)
            ((#\+) (parse 0 1 nil 1 length))
            ((#\-) (parse 0 -1 nil 1 length))
            (t (parse 0 1 nil 0 length)))))))
