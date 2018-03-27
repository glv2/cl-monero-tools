;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defmacro serialize (object specs)
  (loop with result = (gensym)
        with data = (gensym)
        with id = (gensym)
        for spec in specs
        for name = (car spec)
        for writer = (cadr spec)
        for writer-parameters = (cddr spec)
        collect `(let* ((,id ,(intern (string-upcase (symbol-name name)) :keyword))
                        (,data (apply ,writer (geta ,object ,id) (list ,@writer-parameters))))
                   (when ,data
                     (setf ,result (concatenate 'octet-vector ,result ,data))))
          into forms
        finally (return `(let ((,result (make-array 0 :element-type '(unsigned-byte 8))))
                           ,@forms
                           ,result))))

(defmacro serialize-variant (object specs)
  (loop with type = (gensym)
        with thing = (gensym)
        for spec in specs
        for id = (intern (string-upcase (symbol-name (car spec))) :keyword)
        for tag = (cadr spec)
        for writer = (caddr spec)
        for writer-parameters = (cdddr spec)
        collect `((eq ,type ,id)
                  (concatenate 'octet-vector
                               (vector ,tag)
                               (apply ,writer ,thing (list ,@writer-parameters))))
          into forms
        finally (return `(let ((,type (caar ,object))
                               (,thing (cdar object)))
                           (cond ,@forms)))))


;;; Basic types

(defun serialize-single-byte (object)
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element object))

(defun serialize-bytes (object)
  object)

(defun serialize-integer (object)
  (integer->bytes object :varint t))

(defun serialize-vector (objects element-writer &rest element-writer-parameters)
  (let* ((size (length objects))
         (result (serialize-integer size)))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate 'octet-vector result data))))))

(defun serialize-custom-vector (objects element-writer &rest element-writer-parameters)
  (let ((size (length objects))
        (result (make-array 0 :element-type '(unsigned-byte 8))))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate 'octet-vector result data))))))

(defun serialize-byte-vector (object)
  (concatenate 'octet-vector (serialize-integer (length object)) object))

(defun serialize-key (object)
  (serialize-bytes object))

(defun serialize-hash (object)
  (serialize-bytes object))


;;; Transaction outputs

(defun serialize-transaction-output-to-script (object)
  (serialize object
    ((keys #'serialize-vector #'serialize-key)
     (script #'serialize-byte-vector))))

(defun serialize-transaction-output-to-script-hash (object)
  (serialize-hash object))

(defun serialize-transaction-output-to-key (object)
  (serialize-key object))

(defun serialize-transaction-output-target (object)
  (serialize-variant object
    ((script +transaction-output-to-script-tag+
             #'serialize-transaction-output-to-script)
     (script-hash +transaction-output-to-script-hash-tag+
                  #'serialize-transaction-output-to-script-hash)
     (key +transaction-output-to-key-tag+
          #'serialize-transaction-output-to-key))))

(defun serialize-transaction-output (object)
  (serialize object
    ((amount #'serialize-integer)
     (target #'serialize-transaction-output-target))))


;;; Transaction inputs

(defun serialize-transaction-input-generation (object)
  (serialize object
    ((height #'serialize-integer))))

(defun serialize-transaction-input-to-script (object)
  (serialize object
    ((prev #'serialize-hash)
     (prevout #'serialize-integer)
     (sigset #'serialize-byte-vector))))

(defun serialize-transaction-input-to-script-hash (object)
  (serialize object
    ((prev #'serialize-hash)
     (prevout #'serialize-integer)
     (script #'serialize-transaction-output-to-script)
     (sigset #'serialize-byte-vector))))

(defun serialize-transaction-input-to-key (object)
  (serialize object
    ((amount #'serialize-integer)
     (key-offsets #'serialize-vector #'serialize-integer)
     (key-image #'serialize-bytes))))

(defun serialize-transaction-input-target (object)
  (serialize-variant object
    ((generation +transaction-input-generation-tag+
                 #'serialize-transaction-input-generation)
     (script +transaction-input-to-script-tag+
             #'serialize-transaction-input-to-script)
     (script-hash +transaction-input-to-script-hash-tag+
                  #'serialize-transaction-input-to-script-hash)
     (key +transaction-input-to-key-tag+
          #'serialize-transaction-input-to-key))))


;;; Signatures (before ring confidential transaction signatures)

(defun serialize-signature (object)
  (serialize-custom-vector object #'serialize-custom-vector #'serialize-bytes))


;;; Ring confidential transaction signatures

(defun serialize-rct-range-proof (object)
  (labels ((serialize-key64 (object)
             (serialize-custom-vector object #'serialize-key))

           (serialize-boromean-signature (object)
             (serialize object
               ((s0 #'serialize-key64)
                (s1 #'serialize-key64)
                (ee #'serialize-key))))

           (serialize-range-proof (object)
             (serialize object
               ((boromean-signature #'serialize-boromean-signature)
                (pedersen-commitments #'serialize-key64))))

           (serialize-multilayered-group-signature (object)
             (serialize object
               ((ss #'serialize-custom-vector #'serialize-custom-vector #'serialize-key)
                (cc #'serialize-key)))))
    (when object
      (serialize object
        ((range-proofs #'serialize-custom-vector #'serialize-range-proof)
         (multilayered-group-signatures #'serialize-custom-vector
                                        #'serialize-multilayered-group-signature))))))

(defun serialize-rct-bulletproof (object type)
  (flet ((serialize-bulletproof (object)
           (serialize object
             ((a1 #'serialize-key)
              (s #'serialize-key)
              (t1 #'serialize-key)
              (t2 #'serialize-key)
              (taux #'serialize-key)
              (mu #'serialize-key)
              (l #'serialize-vector #'serialize-key)
              (r #'serialize-vector #'serialize-key)
              (a2 #'serialize-key)
              (b #'serialize-key)
              (t #'serialize-key))))

         (serialize-multilayered-group-signature (object)
           (serialize object
             ((ss #'serialize-custom-vector #'serialize-custom-vector #'serialize-key)
              (cc #'serialize-key))))

         (serialize-pseudo-outputs (object type)
           (when (= type +rct-type-simple-bulletproof+)
             (serialize-custom-vector object #'serialize-key))))
    (when object
      (serialize object
        ((bulletproofs #'serialize-custom-vector #'serialize-bulletproof)
         (multilayered-group-signatures #'serialize-custom-vector
                                        #'serialize-multilayered-group-signature)
         (pseudo-outputs #'serialize-pseudo-outputs type))))))

(defun serialize-rct-signature (object)
  (flet ((serialize-pseudo-outputs (object type)
           (when (= type +rct-type-simple+)
             (serialize-custom-vector object #'serialize-key)))

         (serialize-ecdh-tuple (object)
           (serialize object
             ((mask #'serialize-key)
              (amount #'serialize-key)))))
    (let ((type (geta object :type)))
      (concatenate 'octet-vector
                   (vector type)
                   (ecase type
                     ((#.+rct-type-null+)
                      nil)

                     ((#.+rct-type-full+ #.+rct-type-simple+)
                      (serialize object
                        ((fee #'serialize-integer)
                         (pseudo-outputs #'serialize-pseudo-outputs type)
                         (ecdh-info #'serialize-custom-vector #'serialize-ecdh-tuple)
                         (output-public-keys #'serialize-custom-vector #'serialize-key)
                         (rct-signature-prunable #'serialize-rct-range-proof))))

                     ((#.+rct-type-full-bulletproof+ #.+rct-type-simple-bulletproof+)
                      (serialize object
                        ((fee #'serialize-integer)
                         (ecdh-info #'serialize-custom-vector #'serialize-ecdh-tuple)
                         (output-public-keys #'serialize-custom-vector #'serialize-key)
                         (rct-signature-prunable #'serialize-rct-bulletproof type)))))))))


;;; Transaction extra data

(defun serialize-transaction-extra-nonce (object)
  (let* ((type (caar object))
         (data (cdar object))
         (nonce-data (cond ((eq type :payment-id)
                            (concatenate 'octet-vector
                                         (vector +transaction-extra-nonce-payment-id-tag+)
                                         (serialize-bytes data)))
                           ((eq type :encrypted-payment-id)
                            (concatenate 'octet-vector
                                         (vector +transaction-extra-nonce-encrypted-payment-id-tag+)
                                         (serialize-bytes data)))
                           (t
                            (if (> (length data) +transaction-extra-nonce-max-size+)
                                (error "Too much data in nonce")
                                (serialize-bytes data))))))
    (serialize-byte-vector nonce-data)))

(defun serialize-transaction-extra-data-field (object)
  (let ((result (serialize-variant object
                  ((padding +transaction-extra-padding-tag+
                            (lambda (data)
                              (if (> (length data)
                                     +transaction-extra-padding-max-size+)
                                  (error "Too much data in padding.")
                                  (serialize-bytes data))))
                   (transaction-public-key +transaction-extra-public-key-tag+
                                           #'serialize-key)
                   (additional-public-keys +transaction-extra-additional-public-keys-tag+
                                           #'serialize-vector #'serialize-key)
                   (nonce +transaction-extra-nonce-tag+
                          #'serialize-transaction-extra-nonce)))))
    (or result
        (serialize object
          ((data #'serialize-bytes))))))

(defun serialize-transaction-extra-data (object)
  (let ((result #()))
    (dolist (field object)
      (let ((data (serialize-transaction-extra-data-field field)))
        (setf result (concatenate 'octet-vector result data))))
    (serialize-byte-vector result)))


;;; Transactions

(defun serialize-transaction-prefix (object)
  "Return a transaction prefix OBJECT as a byte vector."
  (serialize object
    ((version #'serialize-integer)
     (unlock-time #'serialize-integer)
     (inputs #'serialize-vector #'serialize-transaction-input-target)
     (outputs #'serialize-vector #'serialize-transaction-output)
     (extra #'serialize-transaction-extra-data))))

(defun serialize-transaction (object)
  "Return a transaction OBJECT as a byte vector."
  (let ((version (geta (geta object :prefix) :version)))
    (case version
      ((1) (serialize object
             ((prefix #'serialize-transaction-prefix)
              (signature #'serialize-signature))))
      ((2) (serialize object
             ((prefix #'serialize-transaction-prefix)
              (rct-signature #'serialize-rct-signature))))
      (t (error "Transaction version ~d not supported." version)))))


;;; Blocks

(defun serialize-block-header (object)
  "Return a block header OBJECT as a byte vector."
  (serialize object
    ((major-version #'serialize-integer)
     (minor-version #'serialize-integer)
     (timestamp #'serialize-integer)
     (previous-block-hash #'serialize-hash)
     (nonce #'serialize-bytes))))

(defun serialize-block (object)
  "Return a block OBJECT as a byte vector."
  (serialize object
    ((header #'serialize-block-header)
     (miner-transaction #'serialize-transaction)
     (transaction-hashes #'serialize-vector #'serialize-hash))))
