;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defmacro deserialize (data offset specs)
  (loop with result = (gensym)
        with total-size = (gensym)
        with object = (gensym)
        with size = (gensym)
        for spec in specs
        for name = (car spec)
        for reader = (cadr spec)
        for reader-parameters = (cddr spec)
        collect `(multiple-value-bind (,object ,size)
                     (apply ,reader ,data (+ ,offset ,total-size) (list ,@reader-parameters))
                   (push (cons ,(intern (string-upcase (symbol-name name)) :keyword)
                               ,object)
                         ,result)
                   (incf ,total-size ,size))
          into forms
        finally (return `(let ((,result '())
                               (,total-size 0))
                           ,@forms
                           (values (reverse ,result) ,total-size)))))


;;; Basic types

(defun read-single-byte (data offset)
  (values (aref data offset) 1))

(defun read-bytes (data offset size)
  (values (bytes->hex-string (subseq data offset (+ offset size))) size))

(defun read-varint (data offset)
  "Read a variable length integer starting at OFFSET in DATA.
The second returned value is the size of the integer in bytes."
  (let ((n 0)
        (size 0))
    (do ((l (length data))
         (i offset (1+ i))
         (j 0 (+ j 7)))
        ((= i l))
      (let ((b (aref data i)))
        (incf n (ash (logand b #x7f) j))
        (incf size)
        (when (zerop (logand b #x80))
          (return))))
    (values n size)))

(defun read-vector (data offset element-reader &rest element-reader-parameters)
  (multiple-value-bind (size s0) (read-varint data offset)
    (let ((total-size s0)
          (result (make-array size)))
      (dotimes (i size)
        (multiple-value-bind (e s)
            (apply element-reader data (+ offset total-size) element-reader-parameters)
          (setf (aref result i) e)
          (incf total-size s)))
      (values result total-size))))

(defun read-custom-vector (data offset size element-reader &rest element-reader-parameters)
  (let ((result (make-array size))
        (total-size 0))
    (dotimes (i size)
      (multiple-value-bind (e s)
          (apply element-reader data (+ offset total-size) element-reader-parameters)
        (setf (aref result i) e)
        (incf total-size s)))
    (values result total-size)))

(defun read-byte-vector (data offset)
  (multiple-value-bind (bytes size)
      (read-vector data offset #'read-single-byte)
    (values (bytes->hex-string bytes) size)))

(defun read-key (data offset)
  (read-bytes data offset +ed25519-key-length+))

(defun read-hash (data offset)
  (read-bytes data offset +hash-length+))


;;; Transaction outputs

(defconstant +txout-to-script-tag+ 0)
(defconstant +txout-to-scripthash-tag+ 1)
(defconstant +txout-to-key-tag+ 2)

(defun read-txout-to-script (data offset)
  (deserialize data offset
               ((keys #'read-vector #'read-key)
                (script #'read-byte-vector))))

(defun read-txout-to-scripthash (data offset)
  (read-hash data offset))

(defun read-txout-to-key (data offset)
  (read-key data offset))

(defun read-txout-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +txout-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'read-txout-to-script))))
              ((eq type +txout-to-scripthash-tag+)
               (deserialize data (+ offset 1)
                            ((scripthash #'read-txout-to-scripthash))))
              ((eq type +txout-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'read-txout-to-key)))))
      (values target (+ 1 size)))))

(defun read-txout (data offset)
  (deserialize data offset
               ((amount #'read-varint)
                (target #'read-txout-target))))


;;; Transaction inputs

(defconstant +txin-gen-tag+ 255)
(defconstant +txin-to-script-tag+ 0)
(defconstant +txin-to-scripthash-tag+ 1)
(defconstant +txin-to-key-tag+ 2)

(defun read-txin-gen (data offset)
  (deserialize data offset
               ((height #'read-varint))))

(defun read-txin-to-script(data offset)
  (deserialize data offset
               ((prev #'read-hash)
                (prevout #'read-varint)
                (sigset #'read-byte-vector))))

(defun read-txin-to-scripthash (data offset)
  (deserialize data offset
               ((prev #'read-hash)
                (prevout #'read-varint)
                (script #'read-txout-to-script)
                (sigset #'read-byte-vector))))

(defun read-txin-to-key (data offset)
  (deserialize data offset
               ((amount #'read-varint)
                (key-offsets #'read-vector #'read-varint)
                (k-image #'read-bytes +ed25519-key-length+))))

(defun read-txin-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +txin-gen-tag+)
               (deserialize data (+ offset 1)
                            ((gen #'read-txin-gen))))
              ((eq type +txin-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'read-txin-to-script))))
              ((eq type +txin-to-scripthash-tag+)
               (deserialize data (+ offset 1)
                            ((scripthash #'read-txin-to-scripthash))))
              ((eq type +txin-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'read-txin-to-key)))))
      (values target (+ 1 size)))))


;;; Signatures (before ring confidential transaction signatures)

(defun read-signatures (data offset ring-size vin-size)
  (read-custom-vector data offset vin-size
                      #'read-custom-vector ring-size
                      #'read-bytes (* 2 +ed25519-key-length+)))


;;; Ring confidential transaction signatures

(defconstant +rct-type-null+ 0)
(defconstant +rct-type-full+ 1)
(defconstant +rct-type-simple+ 2)
(defconstant +rct-key-tag+ 144)
(defconstant +rct-key64-tag+ 145)
(defconstant +rct-key-vector-tag+ 146)
(defconstant +rct-key-matrix-tag+ 147)
(defconstant +rct-ctkey-tag+ 148)
(defconstant +rct-ctkey-vector-tag+ 149)
(defconstant +rct-ctkey-matrix-tag+ 150)
(defconstant +rct-ecdh-tuple-tag+ 151)
(defconstant +rct-mg-sig-tag+ 152)
(defconstant +rct-range-sig-tag+ 153)
(defconstant +rct-boro-sig-tag+ 154)
(defconstant +rct-rct-sig-tag+ 155)

(defun read-rct-sig-prunable (data offset ring-size vin-size vout-size type)
  (labels ((read-key64 (data offset)
             (read-custom-vector data offset 64 #'read-key))

           (read-boro-sig (data offset)
             (deserialize data offset
                          ((s0 #'read-key64)
                           (s1 #'read-key64)
                           (ee #'read-key))))

           (read-range-sig (data offset)
             (deserialize data offset
                          ((boro-sig #'read-boro-sig)
                           (ci #'read-key64))))

           (read-mg (data offset)
             (deserialize data offset
                          ((ss #'read-custom-vector ring-size
                               #'read-custom-vector (+ 1 (if (eq type +rct-type-simple+) 1 vin-size))
                               #'read-key)
                           (cc #'read-key)))))
    (deserialize data offset
                 ((range-sigs #'read-custom-vector vout-size #'read-range-sig)
                  (mgs #'read-custom-vector (if (eq type +rct-type-simple+) vin-size 1)
                       #'read-mg)))))

(defun read-rct-signature (data offset ring-size vin-size vout-size)
  (labels ((read-pseudo-outs (data offset type vin-size)
             (if (eq type +rct-type-simple+)
                 (read-custom-vector data offset vin-size #'read-key)
                 (values nil 0)))

           (read-ecdh-tuple (data offset)
             (deserialize data offset
                          ((mask #'read-key)
                           (amount #'read-key)))))
    (let ((type (read-single-byte data offset)))
      (if (eq type +rct-type-null+)
          (values (list (cons :type type)) 1)
          (multiple-value-bind (sig size)
              (deserialize data (+ offset 1)
                           ((txn-fee #'read-varint)
                            (pseudo-outs #'read-pseudo-outs type vin-size)
                            (ecdh-info #'read-custom-vector vout-size #'read-ecdh-tuple)
                            (out-pk #'read-custom-vector vout-size #'read-key)
                            (rct-sig-prunable #'read-rct-sig-prunable
                                              ring-size vin-size vout-size type)))
            (values (append (list (cons :type type)) sig)
                    (+ 1 size)))))))


;;; Transactions

(defconstant +transaction-tag+ 204)

(defun read-transaction-prefix (data offset)
  (deserialize data offset
               ((version #'read-varint)
                (unlock-time #'read-varint)
                (vin #'read-vector #'read-txin-target)
                (vout #'read-vector #'read-txout)
                (extra #'read-byte-vector))))

(defun read-transaction (data offset)
  (multiple-value-bind (prefix prefix-size)
      (deserialize data offset
                   ((prefix #'read-transaction-prefix)))
    (let* ((p (geta prefix :prefix))
           (version (geta p :version))
           (vin (geta p :vin))
           (vin-size (length vin))
           (ring-size (length (geta (geta (aref vin 0) :key) :key-offsets)))
           (vout-size (length (geta p :vout))))
      (multiple-value-bind (signatures signatures-size)
          (if (= 1 version)
              (deserialize data (+ offset prefix-size)
                           ((signatures #'read-signatures ring-size vin-size)))
              (deserialize data (+ offset prefix-size)
                           ((rct-signatures #'read-rct-signature
                                            ring-size vin-size vout-size))))
        (values (append prefix signatures) (+ prefix-size signatures-size))))))


;;; Blocks

(defconstant +block-tag+ 187)

(defun read-block-header (data offset)
  (deserialize data offset
               ((major-version #'read-varint)
                (minor-version #'read-varint)
                (timestamp #'read-varint)
                (prev-id #'read-hash)
                (nonce #'read-bytes 4))))

(defun read-block (data offset)
  (deserialize data offset
               ((header #'read-block-header)
                (miner-tx #'read-transaction)
                (tx-hashes #'read-vector #'read-hash))))
