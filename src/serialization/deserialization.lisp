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
  (bytes->integer data :start offset :varint t))

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
  (deserialize-bytes data offset +key-length+))

(defun deserialize-hash (data offset)
  (deserialize-bytes data offset +hash-length+))


;;; Transaction outputs

(defun deserialize-transaction-output-to-script (data offset)
  (deserialize data offset
               ((keys #'deserialize-vector #'deserialize-key)
                (script #'deserialize-byte-vector))))

(defun deserialize-transaction-output-to-script-hash (data offset)
  (deserialize-hash data offset))

(defun deserialize-transaction-output-to-key (data offset)
  (deserialize-key data offset))

(defun deserialize-transaction-output-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +transaction-output-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'deserialize-transaction-output-to-script))))
              ((eq type +transaction-output-to-script-hash-tag+)
               (deserialize data (+ offset 1)
                            ((script-hash #'deserialize-transaction-output-to-script-hash))))
              ((eq type +transaction-output-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'deserialize-transaction-output-to-key)))))
      (values target (+ 1 size)))))

(defun deserialize-transaction-output (data offset)
  (deserialize data offset
               ((amount #'deserialize-integer)
                (target #'deserialize-transaction-output-target))))


;;; Transaction inputs

(defun deserialize-transaction-input-generation (data offset)
  (deserialize data offset
               ((height #'deserialize-integer))))

(defun deserialize-transaction-input-to-script(data offset)
  (deserialize data offset
               ((prev #'deserialize-hash)
                (prevout #'deserialize-integer)
                (sigset #'deserialize-byte-vector))))

(defun deserialize-transaction-input-to-script-hash (data offset)
  (deserialize data offset
               ((prev #'deserialize-hash)
                (prevout #'deserialize-integer)
                (script #'deserialize-transaction-output-to-script)
                (sigset #'deserialize-byte-vector))))

(defun deserialize-transaction-input-to-key (data offset)
  (deserialize data offset
               ((amount #'deserialize-integer)
                (key-offsets #'deserialize-vector #'deserialize-integer)
                (key-image #'deserialize-bytes +key-length+))))

(defun deserialize-transaction-input-target (data offset)
  (let ((type (aref data offset)))
    (multiple-value-bind (target size)
        (cond ((eq type +transaction-input-generation-tag+)
               (deserialize data (+ offset 1)
                            ((generation #'deserialize-transaction-input-generation))))
              ((eq type +transaction-input-to-script-tag+)
               (deserialize data (+ offset 1)
                            ((script #'deserialize-transaction-input-to-script))))
              ((eq type +transaction-input-to-script-hash-tag+)
               (deserialize data (+ offset 1)
                            ((script-hash #'deserialize-transaction-input-to-script-hash))))
              ((eq type +transaction-input-to-key-tag+)
               (deserialize data (+ offset 1)
                            ((key #'deserialize-transaction-input-to-key)))))
      (values target (+ 1 size)))))


;;; Signatures (before ring confidential transaction signatures)

(defun deserialize-signature (data offset ring-size inputs-size)
  (deserialize-custom-vector data offset inputs-size
                             #'deserialize-custom-vector ring-size
                             #'deserialize-bytes (* 2 +key-length+)))


;;; Ring confidential transaction signatures

(defun deserialize-rct-signature-prunable (data offset ring-size inputs-size outputs-size type)
  (labels ((deserialize-key64 (data offset)
             (deserialize-custom-vector data offset 64 #'deserialize-key))

           (deserialize-boromean-signature (data offset)
             (deserialize data offset
                          ((s0 #'deserialize-key64)
                           (s1 #'deserialize-key64)
                           (ee #'deserialize-key))))

           (deserialize-range-proof (data offset)
             (deserialize data offset
                          ((boromean-signature #'deserialize-boromean-signature)
                           (pedersen-commitments #'deserialize-key64))))

           (deserialize-multilayered-group-signature (data offset)
             (deserialize data offset
                          ((ss #'deserialize-custom-vector ring-size
                               #'deserialize-custom-vector
                               (+ 1 (if (eq type +rct-type-simple+) 1 inputs-size))
                               #'deserialize-key)
                           (cc #'deserialize-key)))))
    (deserialize data offset
                 ((range-proofs #'deserialize-custom-vector outputs-size
                                #'deserialize-range-proof)
                  (multilayered-group-signatures #'deserialize-custom-vector
                                                 (if (eq type +rct-type-simple+) inputs-size 1)
                                                 #'deserialize-multilayered-group-signature)))))

(defun deserialize-rct-signature (data offset ring-size inputs-size outputs-size)
  (flet ((deserialize-pseudo-outputs (data offset type inputs-size)
           (if (eq type +rct-type-simple+)
               (deserialize-custom-vector data offset inputs-size #'deserialize-key)
               (values nil 0)))

         (deserialize-ecdh-tuple (data offset)
           (deserialize data offset
                        ((mask #'deserialize-key)
                         (amount #'deserialize-key)))))
    (let ((type (deserialize-single-byte data offset)))
      (if (eq type +rct-type-null+)
          (values (list (cons :type type)) 1)
          (multiple-value-bind (signature size)
              (deserialize data (+ offset 1)
                           ((fee #'deserialize-integer)
                            (pseudo-outputs #'deserialize-pseudo-outputs type inputs-size)
                            (ecdh-info #'deserialize-custom-vector outputs-size
                                       #'deserialize-ecdh-tuple)
                            (output-public-keys #'deserialize-custom-vector outputs-size
                                                #'deserialize-key)
                            (rct-signature-prunable #'deserialize-rct-signature-prunable
                                                    ring-size inputs-size outputs-size type)))
            (values (append (list (cons :type type)) signature)
                    (+ 1 size)))))))


;;; Transaction extra data

(defun deserialize-transaction-extra-nonce (data offset)
  (multiple-value-bind (nonce-size s0)
      (deserialize-integer data offset)
    (let ((type (aref data (+ offset s0))))
      (multiple-value-bind (nonce s1)
          (cond ((and (eq type +transaction-extra-nonce-payment-id-tag+)
                      (= nonce-size 33))
                 (incf s0)
                 (deserialize data (+ offset s0)
                              ((payment-id #'deserialize-bytes 32))))
                ((and (eq type +transaction-extra-nonce-encrypted-payment-id-tag+)
                      (= nonce-size 9))
                 (incf s0)
                 (deserialize data (+ offset s0)
                              ((encrypted-payment-id #'deserialize-bytes 8))))
                (t
                 (deserialize data (+ offset s0)
                              ((data #'deserialize-bytes
                                     (min nonce-size
                                          +transaction-extra-nonce-max-size+))))))
        (values nonce (+ s0 s1))))))

(defun deserialize-transaction-extra-data-field (data offset max-size)
  (multiple-value-bind (type type-size)
      (deserialize-integer data offset)
    (multiple-value-bind (field field-size)
        (cond ((eq type +transaction-extra-padding-tag+)
               (deserialize data (+ offset type-size)
                            ((padding #'deserialize-bytes
                                      (min (- max-size type-size)
                                           +transaction-extra-padding-max-size+)))))
              ((eq type +transaction-extra-public-key-tag+)
               (deserialize data (+ offset type-size)
                            ((transaction-public-key #'deserialize-key))))
              ((eq type +transaction-extra-nonce-tag+)
               (deserialize data (+ offset type-size)
                            ((nonce #'deserialize-transaction-extra-nonce)))))
      (if field
          (values field (+ type-size field-size))
          (deserialize data offset
                       ((data #'deserialize-bytes max-size)))))))

(defun deserialize-transaction-extra-data (data offset)
  (multiple-value-bind (extra-data extra-data-size)
      (deserialize-byte-vector data offset)
    (do ((fields-size (- extra-data-size (nth-value 1 (deserialize-integer data offset))))
         (fields '())
         (field-offset 0))
        ((>= field-offset fields-size) (values (reverse fields) extra-data-size))
      (multiple-value-bind (field field-size)
          (deserialize-transaction-extra-data-field extra-data field-offset
                                                    (- fields-size field-offset))
        (push field fields)
        (incf field-offset field-size)))))


;;; Transactions

(defun deserialize-transaction-prefix (data offset)
  "Return the transaction prefix whose serialization starts at OFFSET
in DATA. The second returned value in the size of the serialized
transaction prefix."
  (deserialize data offset
               ((version #'deserialize-integer)
                (unlock-time #'deserialize-integer)
                (inputs #'deserialize-vector #'deserialize-transaction-input-target)
                (outputs #'deserialize-vector #'deserialize-transaction-output)
                (extra #'deserialize-transaction-extra-data))))

(defun deserialize-transaction (data offset)
  "Return the transaction whose serialization starts at OFFSET in DATA.
The second returned value in the size of the serialized transaction."
  (multiple-value-bind (prefix prefix-size)
      (deserialize data offset
                   ((prefix #'deserialize-transaction-prefix)))
    (let* ((transaction-prefix (geta prefix :prefix))
           (version (geta transaction-prefix :version))
           (inputs (geta transaction-prefix :inputs))
           (inputs-size (length inputs))
           (ring-size (if (plusp (length inputs))
                          (length (geta (geta (aref inputs 0) :key) :key-offsets))
                          0))
           (outputs-size (length (geta transaction-prefix :outputs))))
      (multiple-value-bind (signature signature-size)
          (case version
            ((1) (deserialize data (+ offset prefix-size)
                              ((signature #'deserialize-signature ring-size inputs-size))))
            ((2) (deserialize data (+ offset prefix-size)
                              ((rct-signature #'deserialize-rct-signature
                                              ring-size inputs-size outputs-size))))
            (t (error "Transaction version ~d not supported." version)))
        (values (append prefix signature) (+ prefix-size signature-size))))))


;;; Blocks

(defun deserialize-block-header (data offset)
  "Return the block header whose serialization starts at OFFSET in
DATA. The second returned value in the size of the serialized block
header."
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
