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

(defun serialize-txout-to-script (object)
  (serialize object
             ((keys #'serialize-vector #'serialize-key)
              (script #'serialize-byte-vector))))

(defun serialize-txout-to-scripthash (object)
  (serialize-hash object))

(defun serialize-txout-to-key (object)
  (serialize-key object))

(defun serialize-txout-target (object)
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-script-tag+)
                        (serialize-txout-to-script target)))
          ((eq type :scripthash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-scripthash-tag+)
                        (serialize-txout-to-scripthash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-key-tag+)
                        (serialize-txout-to-key target))))))

(defun serialize-txout (object)
  (serialize object
             ((amount #'serialize-integer)
              (target #'serialize-txout-target))))


;;; Transaction inputs

(defun serialize-txin-gen (object)
  (serialize object
             ((height #'serialize-integer))))

(defun serialize-txin-to-script (object)
  (serialize object
             ((prev #'serialize-hash)
              (prevout #'serialize-integer)
              (sigset #'serialize-byte-vector))))

(defun serialize-txin-to-scripthash (object)
  (serialize object
             ((prev #'serialize-hash)
              (prevout #'serialize-integer)
              (script #'serialize-txout-to-script)
              (sigset #'serialize-byte-vector))))

(defun serialize-txin-to-key (object)
  (serialize object
             ((amount #'serialize-integer)
              (key-offsets #'serialize-vector #'serialize-integer)
              (key-image #'serialize-bytes))))

(defun serialize-txin-target (object)
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :gen)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-gen-tag+)
                        (serialize-txin-gen target)))
          ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-script-tag+)
                        (serialize-txin-to-script target)))
          ((eq type :scripthash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-scripthash-tag+)
                        (serialize-txin-to-scripthash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-key-tag+)
                        (serialize-txin-to-key target))))))


;;; Signatures (before ring confidential transaction signatures)

(defun serialize-signatures (object)
  (serialize-custom-vector object #'serialize-custom-vector #'serialize-bytes))


;;; Ring confidential transaction signatures

(defun serialize-rct-sig-prunable (object)
  (labels ((serialize-key64 (object)
             (serialize-custom-vector object #'serialize-key))

           (serialize-boro-sig (object)
             (serialize object
                        ((s0 #'serialize-key64)
                         (s1 #'serialize-key64)
                         (ee #'serialize-key))))

           (serialize-range-sig (object)
             (serialize object
                        ((boro-sig #'serialize-boro-sig)
                         (ci #'serialize-key64))))

           (serialize-mg (object)
             (serialize object
                        ((ss #'serialize-custom-vector #'serialize-custom-vector #'serialize-key)
                         (cc #'serialize-key)))))
    (when object
      (serialize object
                 ((range-sigs #'serialize-custom-vector #'serialize-range-sig)
                  (mgs #'serialize-custom-vector #'serialize-mg))))))

(defun serialize-rct-signatures (object)
  (flet ((serialize-pseudo-outputs (object type)
           (unless (eq type +rct-type-simple+)
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
                                 (out-pk #'serialize-custom-vector #'serialize-key)
                                 (rct-sig-prunable #'serialize-rct-sig-prunable))))))))


;;; Transactions

(defun serialize-transaction-prefix (object)
  (serialize object
             ((version #'serialize-integer)
              (unlock-time #'serialize-integer)
              (inputs #'serialize-vector #'serialize-txin-target)
              (outputs #'serialize-vector #'serialize-txout)
              (extra #'serialize-byte-vector))))

(defun serialize-transaction (object)
  (let ((version (geta (geta object :prefix) :version)))
    (if (= 1 version)
        (serialize object
                   ((prefix #'serialize-transaction-prefix)
                    (signatures #'serialize-signatures)))
        (serialize object
                   ((prefix #'serialize-transaction-prefix)
                    (rct-signatures #'serialize-rct-signatures))))))


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
