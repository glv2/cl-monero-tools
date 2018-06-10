;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun lisp-type->storage-type (type)
  (cond
    ((subtypep type '(signed-byte 8)) +portable-storage-type-int8+)
    ((subtypep type '(unsigned-byte 8)) +portable-storage-type-uint8+)
    ((subtypep type '(signed-byte 16)) +portable-storage-type-int16+)
    ((subtypep type '(unsigned-byte 16)) +portable-storage-type-uint16+)
    ((subtypep type '(signed-byte 32)) +portable-storage-type-int32+)
    ((subtypep type '(unsigned-byte 32)) +portable-storage-type-uint32+)
    ((subtypep type '(signed-byte 64)) +portable-storage-type-int64+)
    ((subtypep type '(unsigned-byte 64)) +portable-storage-type-uint64+)
    ((subtypep type 'float) +portable-storage-type-double+)
    ((subtypep type 'string) +portable-storage-type-string+)
    ((subtypep type 'boolean) +portable-storage-type-bool+)
    ((subtypep type 'list) +portable-storage-type-object+)
    ((subtypep type 'vector) +portable-storage-type-array+)
    (t (error "couldn't determine storage type for lisp type: ~a" type))))

(defun storage-type->lisp-type (type)
  (ecase type
    ((#.+portable-storage-type-int64+) '(signed-byte 64))
    ((#.+portable-storage-type-int32+) '(signed-byte 32))
    ((#.+portable-storage-type-int16+) '(signed-byte 16))
    ((#.+portable-storage-type-int8+) '(signed-byte 8))
    ((#.+portable-storage-type-uint64+) '(unsigned-byte 64))
    ((#.+portable-storage-type-uint32+) '(unsigned-byte 32))
    ((#.+portable-storage-type-uint16+) '(unsigned-byte 16))
    ((#.+portable-storage-type-uint8+) '(unsigned-byte 8))
    ((#.+portable-storage-type-double+) 'double-float)
    ((#.+portable-storage-type-string+) 'simple-string)
    ((#.+portable-storage-type-bool+) 'boolean)
    ((#.+portable-storage-type-object+) 'list)
    ((#.+portable-storage-type-array+) 'simple-vector)))

(defun storage-type->serialization-function (type)
  (ecase type
    ((#.+portable-storage-type-int64+) #'storage-serialize-int64)
    ((#.+portable-storage-type-int32+) #'storage-serialize-int32)
    ((#.+portable-storage-type-int16+) #'storage-serialize-int16)
    ((#.+portable-storage-type-int8+)  #'storage-serialize-int8)
    ((#.+portable-storage-type-uint64+) #'storage-serialize-uint64)
    ((#.+portable-storage-type-uint32+) #'storage-serialize-uint32)
    ((#.+portable-storage-type-uint16+) #'storage-serialize-uint16)
    ((#.+portable-storage-type-uint8+) #'storage-serialize-uint8)
    ((#.+portable-storage-type-double+) #'storage-serialize-double-float)
    ((#.+portable-storage-type-string+) #'storage-serialize-string)
    ((#.+portable-storage-type-bool+) #'storage-serialize-boolean)
    ((#.+portable-storage-type-object+) #'storage-serialize-section)
    ((#.+portable-storage-type-array+) #'storage-serialize-vector)))

(defun storage-type->deserialization-function (type)
  (ecase type
    ((#.+portable-storage-type-int64+) #'storage-deserialize-int64)
    ((#.+portable-storage-type-int32+) #'storage-deserialize-int32)
    ((#.+portable-storage-type-int16+) #'storage-deserialize-int16)
    ((#.+portable-storage-type-int8+)  #'storage-deserialize-int8)
    ((#.+portable-storage-type-uint64+) #'storage-deserialize-uint64)
    ((#.+portable-storage-type-uint32+) #'storage-deserialize-uint32)
    ((#.+portable-storage-type-uint16+) #'storage-deserialize-uint16)
    ((#.+portable-storage-type-uint8+) #'storage-deserialize-uint8)
    ((#.+portable-storage-type-double+) #'storage-deserialize-double-float)
    ((#.+portable-storage-type-string+) #'storage-deserialize-string)
    ((#.+portable-storage-type-bool+) #'storage-deserialize-boolean)
    ((#.+portable-storage-type-object+) #'storage-deserialize-section)
    ((#.+portable-storage-type-array+) #'storage-deserialize-vector)))

(defun storage-serialize-varint (n)
  "Return the variable size integer encoding of an integer N."
  (cond
    ((<= n 63)
     (integer->bytes (logior (ash n 2) +portable-storage-raw-size-byte+) :size 1))
    ((<= n 16383)
     (integer->bytes (logior (ash n 2) +portable-storage-raw-size-word+) :size 2))
    ((<= n 1073741823)
     (integer->bytes (logior (ash n 2) +portable-storage-raw-size-double-word+) :size 4))
    ((<= n 4611686018427387903)
     (integer->bytes (logior (ash n 2) +portable-storage-raw-size-quad-word+) :size 8))
    (t
     (error "~d is too big to be serialized." n))))

(defun storage-deserialize-varint (bytes &key (start 0))
  "Read the variable size integer encoded in BYTES."
  (let* ((n (ash (aref bytes start) -2))
         (size (ldb (byte 2 0) (aref bytes start)))
         (end (+ start (ecase size
                         ((#.+portable-storage-raw-size-byte+) 1)
                         ((#.+portable-storage-raw-size-word+) 2)
                         ((#.+portable-storage-raw-size-double-word+) 4)
                         ((#.+portable-storage-raw-size-quad-word+) 8))))
         (x (bytes->integer bytes :start (+ start 1) :end end)))
    (values (logior n (ash x 6)) (- end start))))

(defun storage-serialize-int64 (object &key in-vector)
  (let ((n (if (minusp object)
               (+ 18446744073709551616 object)
               object)))
    (concatenate 'octet-vector
                 (unless in-vector (vector +portable-storage-type-int64+))
                 (integer->bytes n :size 8))))

(defun storage-deserialize-int64 (data offset)
  (let* ((n (bytes->integer data :start offset :end (+ offset 8)))
         (negative (logbitp 63 n)))
    (values (if negative (- n 18446744073709551616) n)
            8)))

(defun storage-serialize-int32 (object &key in-vector)
  (let ((n (if (minusp object)
               (+ 4294967296 object)
               object)))
    (concatenate 'octet-vector
                 (unless in-vector (vector +portable-storage-type-int32+))
                 (integer->bytes n :size 4))))

(defun storage-deserialize-int32 (data offset)
  (let* ((n (bytes->integer data :start offset :end (+ offset 4)))
         (negative (logbitp 31 n)))
    (values (if negative (- n 4294967296) n)
            4)))

(defun storage-serialize-int16 (object &key in-vector)
  (let ((n (if (minusp object)
               (+ 65536 object)
               object)))
    (concatenate 'octet-vector
                 (unless in-vector (vector +portable-storage-type-int8+))
                 (integer->bytes n :size 2))))

(defun storage-deserialize-int16 (data offset)
  (let* ((n (bytes->integer data :start offset :end (+ offset 2)))
         (negative (logbitp 15 n)))
    (values (if negative (- n 65536) n)
            2)))

(defun storage-serialize-int8 (object &key in-vector)
  (let ((n (if (minusp object)
               (+ 256 object)
               object)))
    (concatenate 'octet-vector
                 (unless in-vector (vector +portable-storage-type-int8+))
                 (integer->bytes n :size 1))))

(defun storage-deserialize-int8 (data offset)
  (let* ((n (bytes->integer data :start offset :end (+ offset 1)))
         (negative (logbitp 7 n)))
    (values (if negative (- n 256) n)
            1)))

(defun storage-serialize-uint64 (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-uint64+))
               (integer->bytes object :size 8)))

(defun storage-deserialize-uint64 (data offset)
  (bytes->integer data :start offset :end (+ offset 8)))

(defun storage-serialize-uint32 (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-uint32+))
               (integer->bytes object :size 4)))

(defun storage-deserialize-uint32 (data offset)
  (bytes->integer data :start offset :end (+ offset 4)))

(defun storage-serialize-uint16 (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-uint16+))
               (integer->bytes object :size 2)))

(defun storage-deserialize-uint16 (data offset)
  (bytes->integer data :start offset :end (+ offset 2)))

(defun storage-serialize-uint8 (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-uint8+))
               (integer->bytes object :size 1)))

(defun storage-deserialize-uint8 (data offset)
  (bytes->integer data :start offset :end (+ offset 1)))

(defun storage-serialize-double-float (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-double+))
               (integer->bytes (encode-float64 object))))

(defun storage-deserialize-double-float (data offset)
  (values (decode-float64 (bytes->integer data :start offset :end (+ offset 8)))
          8))

(defun storage-serialize-boolean (object &key in-vector)
  (concatenate 'octet-vector
               (unless in-vector (vector +portable-storage-type-bool+))
               (vector (if object 1 0))))

(defun storage-deserialize-boolean (data offset)
  (values (plusp (aref data offset))
          1))

(defun storage-serialize-vector (objects &key in-vector)
  (with-octet-output-stream (result)
    (let* ((size (length objects))
           (type (lisp-type->storage-type (let ((type (array-element-type objects)))
                                            (if (eq type 't)
                                                (type-of (aref objects 0))
                                                type))))
           (serialization-function (storage-type->serialization-function type)))
      (unless in-vector
        (write-byte (logior type +portable-storage-array-flag+) result))
      (write-sequence (storage-serialize-varint size) result)
      (dotimes (i size)
        (let ((data (funcall serialization-function (aref objects i) :in-vector t)))
          (write-sequence data result))))))

(defun storage-deserialize-vector (data offset type)
  (multiple-value-bind (size s0)
      (storage-deserialize-varint data :start offset)
    (let* ((lisp-type (storage-type->lisp-type type))
           (deserialization-function (storage-type->deserialization-function type))
           (result (make-array size :element-type lisp-type)))
       (dotimes (i size)
        (multiple-value-bind (thing s1)
            (funcall deserialization-function data (+ offset s0))
          (setf (aref result i) thing)
          (incf s0 s1)))
      (values result s0))))

(defun storage-serialize-string (object &key in-vector)
  (let ((data (string->bytes object)))
    (concatenate 'octet-vector
                 (unless in-vector (vector +portable-storage-type-string+))
                 (storage-serialize-varint (length data))
                 data)))

(defun storage-deserialize-string (data offset)
  (multiple-value-bind (size s0)
      (storage-deserialize-varint data :start offset)
    (values (bytes->string data :start (+ offset s0) :end (+ offset s0 size))
            (+ s0 size))))

(defun storage-serialize-section (object &key in-vector)
  (with-octet-output-stream (result)
    (let ((size (length object)))
      (unless in-vector
        (write-byte +portable-storage-type-object+ result))
      (write-sequence (storage-serialize-varint size) result)
      (dolist (pair object)
        (let ((name (string->bytes (lisp-name->json-name (symbol-name (car pair)))))
              (thing (cdr pair)))
          (when (> (length name) 255)
            (error "storage entry name is too long: ~a" name))
          (write-byte (length name) result)
          (write-sequence name result)
          (write-sequence (storage-serialize thing) result))))))

(defun storage-deserialize-section (data offset)
  (declare (optimize (debug 3)))
  (multiple-value-bind (size s0)
      (storage-deserialize-varint data :start offset)
    (let ((result '()))
      (dotimes (i size)
        (let* ((name-size (aref data (+ offset s0)))
               (name (intern (json-name->lisp-name (bytes->string (subseq data
                                                                          (+ offset s0 1)
                                                                          (+ offset s0 1 name-size))))
                             :keyword)))
            (multiple-value-bind (thing s1)
                (storage-deserialize data (+ offset s0 1 name-size))
              (push (cons name thing) result)
              (incf s0 (+ 1 name-size s1)))))
      (values (reverse result) s0))))

(defun storage-serialize (object)
  (multiple-value-bind (object lisp-type)
      (if (and (consp object) (numberp (car object)))
          (values (car object) (cdr object))
          (values object (type-of object)))
    (funcall (storage-type->serialization-function (lisp-type->storage-type lisp-type)) object)))

(defun storage-deserialize (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (result s0)
        (if (zerop (logand type +portable-storage-array-flag+))
            (funcall (storage-type->deserialization-function type) data (+ offset 1))
            (storage-deserialize-vector data (+ offset 1) (logxor type +portable-storage-array-flag+)))
      (values result (+ 1 s0)))))

(defun serialize-to-binary-storage (object)
  "Return an OBJECT as a byte vector."
  (concatenate 'octet-vector
               +portable-storage-header+
               (storage-serialize-section object :in-vector t)))

(defun deserialize-from-binary-storage (data offset)
  "Return the object whose serialization starts at OFFSET in DATA.
The second returned value is the size of the serialized object."
  (let* ((header-length (length +portable-storage-header+))
         (header (subseq data offset (+ offset header-length))))
    (unless (equalp header +portable-storage-header+)
      (error "invalid binary storage header"))
    (storage-deserialize-section data (+ offset header-length))))
