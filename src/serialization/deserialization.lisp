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

(defun deserialize-single-byte (data offset)
  (values (aref data offset) 1))

(defun deserialize-bytes (data offset size)
  (values (subseq data offset (+ offset size)) size))

(defun deserialize-integer (data offset)
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

(defun deserialize-vector (data offset element-reader &rest element-reader-parameters)
  (multiple-value-bind (size s0)
      (deserialize-integer data offset)
    (let ((total-size s0)
          (result (make-array size)))
      (dotimes (i size)
        (multiple-value-bind (e s)
            (apply element-reader data (+ offset total-size) element-reader-parameters)
          (setf (aref result i) e)
          (incf total-size s)))
      (values result total-size))))

(defun deserialize-custom-vector (data offset size element-reader &rest element-reader-parameters)
  (let ((result (make-array size))
        (total-size 0))
    (dotimes (i size)
      (multiple-value-bind (e s)
          (apply element-reader data (+ offset total-size) element-reader-parameters)
        (setf (aref result i) e)
        (incf total-size s)))
    (values result total-size)))

(defun deserialize-byte-vector (data offset)
  (multiple-value-bind (size s0)
      (deserialize-integer data offset)
    (let ((bytes (subseq data (+ offset s0) (+ offset s0 size))))
      (values bytes (+ s0 size)))))

(defun deserialize-key (data offset)
  (deserialize-bytes data offset +ed25519-key-length+))

(defun deserialize-hash (data offset)
  (deserialize-bytes data offset +hash-length+))


;;; Transaction outputs

(defun deserialize-txout-to-script (data offset)
  (deserialize data offset
               ((keys #'deserialize-vector #'deserialize-key)
                (script #'deserialize-byte-vector))))

(defun deserialize-txout-to-scripthash (data offset)
  (deserialize-hash data offset))

(defun deserialize-txout-to-key (data offset)
  (deserialize-key data offset))

(defun deserialize-txout-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +txout-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'deserialize-txout-to-script))))
              ((eq type +txout-to-scripthash-tag+)
               (deserialize data (+ offset 1)
                            ((scripthash #'deserialize-txout-to-scripthash))))
              ((eq type +txout-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'deserialize-txout-to-key)))))
      (values target (+ 1 size)))))

(defun deserialize-txout (data offset)
  (deserialize data offset
               ((amount #'deserialize-integer)
                (target #'deserialize-txout-target))))


;;; Transaction inputs

(defun deserialize-txin-gen (data offset)
  (deserialize data offset
               ((height #'deserialize-integer))))

(defun deserialize-txin-to-script(data offset)
  (deserialize data offset
               ((prev #'deserialize-hash)
                (prevout #'deserialize-integer)
                (sigset #'deserialize-byte-vector))))

(defun deserialize-txin-to-scripthash (data offset)
  (deserialize data offset
               ((previous #'deserialize-hash)
                (prevout #'deserialize-integer)
                (script #'deserialize-txout-to-script)
                (sigset #'deserialize-byte-vector))))

(defun deserialize-txin-to-key (data offset)
  (deserialize data offset
               ((amount #'deserialize-integer)
                (key-offsets #'deserialize-vector #'deserialize-integer)
                (key-image #'deserialize-bytes +ed25519-key-length+))))

(defun deserialize-txin-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +txin-gen-tag+)
               (deserialize data (+ offset 1)
                            ((gen #'deserialize-txin-gen))))
              ((eq type +txin-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'deserialize-txin-to-script))))
              ((eq type +txin-to-scripthash-tag+)
               (deserialize data (+ offset 1)
                            ((scripthash #'deserialize-txin-to-scripthash))))
              ((eq type +txin-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'deserialize-txin-to-key)))))
      (values target (+ 1 size)))))


;;; Signatures (before ring confidential transaction signatures)

(defun deserialize-signatures (data offset ring-size vin-size)
  (deserialize-custom-vector data offset vin-size
                      #'deserialize-custom-vector ring-size
                      #'deserialize-bytes (* 2 +ed25519-key-length+)))


;;; Ring confidential transaction signatures

(defun deserialize-rct-sig-prunable (data offset ring-size vin-size vout-size type)
  (labels ((deserialize-key64 (data offset)
             (deserialize-custom-vector data offset 64 #'deserialize-key))

           (deserialize-boro-sig (data offset)
             (deserialize data offset
                          ((s0 #'deserialize-key64)
                           (s1 #'deserialize-key64)
                           (ee #'deserialize-key))))

           (deserialize-range-sig (data offset)
             (deserialize data offset
                          ((boro-sig #'deserialize-boro-sig)
                           (ci #'deserialize-key64))))

           (deserialize-mg (data offset)
             (deserialize data offset
                          ((ss #'deserialize-custom-vector ring-size
                               #'deserialize-custom-vector (+ 1 (if (eq type +rct-type-simple+) 1 vin-size))
                               #'deserialize-key)
                           (cc #'deserialize-key)))))
    (deserialize data offset
                 ((range-sigs #'deserialize-custom-vector vout-size #'deserialize-range-sig)
                  (mgs #'deserialize-custom-vector (if (eq type +rct-type-simple+) vin-size 1)
                       #'deserialize-mg)))))

(defun deserialize-rct-signatures (data offset ring-size vin-size vout-size)
  (flet ((deserialize-pseudo-outputs (data offset type vin-size)
           (if (eq type +rct-type-simple+)
               (deserialize-custom-vector data offset vin-size #'deserialize-key)
               (values nil 0)))

         (deserialize-ecdh-tuple (data offset)
           (deserialize data offset
                        ((mask #'deserialize-key)
                         (amount #'deserialize-key)))))
    (let ((type (deserialize-single-byte data offset)))
      (if (eq type +rct-type-null+)
          (values (list (cons :type type)) 1)
          (multiple-value-bind (sig size)
              (deserialize data (+ offset 1)
                           ((fee #'deserialize-integer)
                            (pseudo-outputs #'deserialize-pseudo-outputs type vin-size)
                            (ecdh-info #'deserialize-custom-vector vout-size #'deserialize-ecdh-tuple)
                            (out-pk #'deserialize-custom-vector vout-size #'deserialize-key)
                            (rct-sig-prunable #'deserialize-rct-sig-prunable
                                              ring-size vin-size vout-size type)))
            (values (append (list (cons :type type)) sig)
                    (+ 1 size)))))))


;;; Transactions

(defun deserialize-transaction-prefix (data offset)
  (deserialize data offset
               ((version #'deserialize-integer)
                (unlock-time #'deserialize-integer)
                (inputs #'deserialize-vector #'deserialize-txin-target)
                (outputs #'deserialize-vector #'deserialize-txout)
                (extra #'deserialize-byte-vector))))

(defun deserialize-transaction (data offset)
  "Return the transaction whose serialization starts at OFFSET in DATA.
The second returned value in the size of the serialized transaction."
  (multiple-value-bind (prefix prefix-size)
      (deserialize data offset
                   ((prefix #'deserialize-transaction-prefix)))
    (let* ((p (geta prefix :prefix))
           (version (geta p :version))
           (vin (geta p :inputs))
           (vin-size (length vin))
           (ring-size (if vin
                          (length (geta (geta (aref vin 0) :key) :key-offsets))
                          0))
           (vout-size (length (geta p :outputs))))
      (multiple-value-bind (signatures signatures-size)
          (if (= 1 version)
              (deserialize data (+ offset prefix-size)
                           ((signatures #'deserialize-signatures ring-size vin-size)))
              (deserialize data (+ offset prefix-size)
                           ((rct-signatures #'deserialize-rct-signatures
                                            ring-size vin-size vout-size))))
        (values (append prefix signatures) (+ prefix-size signatures-size))))))


;;; Blocks

(defun deserialize-block-header (data offset)
  (deserialize data offset
               ((major-version #'deserialize-integer)
                (minor-version #'deserialize-integer)
                (timestamp #'deserialize-integer)
                (previous-block-hash #'deserialize-hash)
                (nonce #'deserialize-bytes 4))))

(defun deserialize-block (data offset)
  "Return the block whose serialization starts at OFFSET in DATA.
The second returned value in the size of the serialized block."
  (deserialize data offset
               ((header #'deserialize-block-header)
                (miner-transaction #'deserialize-transaction)
                (transaction-hashes #'deserialize-vector #'deserialize-hash))))
