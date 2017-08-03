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
                     (setf ,result (concatenate '(simple-array (unsigned-byte 8) (*))
                                                ,result
                                                ,data))))
          into forms
        finally (return `(let ((,result (make-array 0 :element-type '(unsigned-byte 8))))
                           ,@forms
                           ,result))))


;;; Basic types

(defun serialize-single-byte (object)
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element object))

(defun serialize-bytes (object)
  object)

(defun serialize-integer (object)
  (let* ((size (max 1 (ceiling (integer-length object) 7)))
         (data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size data)
      (setf (aref data i) (logior (logand object #x7f) (if (< i (- size 1)) #x80 0)))
      (setf object (ash object -7)))))

(defun serialize-vector (objects element-writer &rest element-writer-parameters)
  (let* ((size (length objects))
         (result (serialize-integer size)))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate '(simple-array (unsigned-byte 8) (*)) result data))))))

(defun serialize-custom-vector (objects element-writer &rest element-writer-parameters)
  (let ((size (length objects))
        (result (make-array 0 :element-type '(unsigned-byte 8))))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate '(simple-array (unsigned-byte 8) (*)) result data))))))

(defun serialize-byte-vector (object)
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (serialize-integer (length object))
               object))

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
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-output-to-script-tag+)
                        (serialize-transaction-output-to-script target)))
          ((eq type :script-hash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-output-to-script-hash-tag+)
                        (serialize-transaction-output-to-script-hash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-output-to-key-tag+)
                        (serialize-transaction-output-to-key target))))))

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
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :generation)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-input-generation-tag+)
                        (serialize-transaction-input-generation target)))
          ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-input-to-script-tag+)
                        (serialize-transaction-input-to-script target)))
          ((eq type :script-hash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-input-to-script-hash-tag+)
                        (serialize-transaction-input-to-script-hash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +transaction-input-to-key-tag+)
                        (serialize-transaction-input-to-key target))))))


;;; Signatures (before ring confidential transaction signatures)

(defun serialize-signature (object)
  (serialize-custom-vector object #'serialize-custom-vector #'serialize-bytes))


;;; Ring confidential transaction signatures

(defun serialize-rct-signature-prunable (object)
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

(defun serialize-rct-signature (object)
  (flet ((serialize-pseudo-outputs (object type)
           (when (eq type +rct-type-simple+)
             (serialize-custom-vector object #'serialize-key)))

         (serialize-ecdh-tuple (object)
           (serialize object
                      ((mask #'serialize-key)
                       (amount #'serialize-key)))))
    (let ((type (geta object :type)))
      (concatenate '(simple-array (unsigned-byte 8) (*))
                   (vector type)
                   (unless (eq type +rct-type-null+)
                     (serialize object
                                ((fee #'serialize-integer)
                                 (pseudo-outputs #'serialize-pseudo-outputs type)
                                 (ecdh-info #'serialize-custom-vector #'serialize-ecdh-tuple)
                                 (output-public-keys #'serialize-custom-vector #'serialize-key)
                                 (rct-signature-prunable #'serialize-rct-signature-prunable))))))))


;;; Transactions

(defun serialize-transaction-prefix (object)
  (serialize object
             ((version #'serialize-integer)
              (unlock-time #'serialize-integer)
              (inputs #'serialize-vector #'serialize-transaction-input-target)
              (outputs #'serialize-vector #'serialize-transaction-output)
              (extra #'serialize-byte-vector))))

(defun serialize-transaction (object)
  (let ((version (geta (geta object :prefix) :version)))
    (if (= 1 version)
        (serialize object
                   ((prefix #'serialize-transaction-prefix)
                    (signature #'serialize-signature)))
        (serialize object
                   ((prefix #'serialize-transaction-prefix)
                    (rct-signature #'serialize-rct-signature))))))


;;; Blocks

(defun serialize-block-header (object)
  (serialize object
             ((major-version #'serialize-integer)
              (minor-version #'serialize-integer)
              (timestamp #'serialize-integer)
              (previous-block-hash #'serialize-hash)
              (nonce #'serialize-bytes))))

(defun serialize-block (object)
  (serialize object
             ((header #'serialize-block-header)
              (miner-transaction #'serialize-transaction)
              (transaction-hashes #'serialize-vector #'serialize-hash))))
