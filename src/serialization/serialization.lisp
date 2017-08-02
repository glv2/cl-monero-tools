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
                   (setf ,result (concatenate '(simple-array (unsigned-byte 8) (*))
                                              ,result
                                              ,data)))
          into forms
        finally (return `(let ((,result #()))
                           ,@forms
                           ,result))))


;;; Basic types

(defun write-single-byte (object)
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element object))

(defun write-bytes (object)
  (hex-string->bytes object))

(defun write-varint (object)
  (let* ((size (max 1 (ceiling (integer-length object) 7)))
         (data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size data)
      (setf (aref data i) (logior (logand object #x7f) (if (< i (- size 1)) #x80 0)))
      (setf object (ash object -7)))))

(defun write-vector (objects element-writer &rest element-writer-parameters)
  (let* ((size (length objects))
         (result (write-varint size)))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate '(simple-array (unsigned-byte 8) (*)) result data))))))

(defun write-custom-vector (objects element-writer &rest element-writer-parameters)
  (let ((size (length objects))
        (result #()))
    (dotimes (i size result)
      (let ((data (apply element-writer (aref objects i) element-writer-parameters)))
        (setf result (concatenate '(simple-array (unsigned-byte 8) (*)) result data))))))

(defun write-byte-vector (object)
  (write-vector (hex-string->bytes object) #'write-single-byte))

(defun write-key (object)
  (write-bytes object))

(defun write-hash (object)
  (write-bytes object))


;;; Transaction outputs

(defun write-txout-to-script (object)
  (serialize object
             ((keys #'write-vector #'write-key)
              (script #'write-byte-vector))))

(defun write-txout-to-scripthash (object)
  (write-hash object))

(defun write-txout-to-key (object)
  (write-key object))

(defun write-txout-target (object)
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-script-tag+)
                        (write-txout-to-script target)))
          ((eq type :scripthash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-scripthash-tag+)
                        (write-txout-to-scripthash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txout-to-key-tag+)
                        (write-txout-to-key target))))))

(defun write-txout (object)
  (serialize object
             ((amount #'write-varint)
              (target #'write-txout-target))))


;;; Transaction inputs

(defun write-txin-gen (object)
  (serialize object
             ((height #'write-varint))))

(defun write-txin-to-script (object)
  (serialize object
             ((prev #'write-hash)
              (prevout #'write-varint)
              (sigset #'write-byte-vector))))

(defun write-txin-to-scripthash (object)
  (serialize object
             ((prev #'write-hash)
              (prevout #'write-varint)
              (script #'write-txout-to-script)
              (sigset #'write-byte-vector))))

(defun write-txin-to-key (object)
  (serialize object
             ((amount #'write-varint)
              (key-offsets #'write-vector #'write-varint)
              (key-image #'write-bytes))))

(defun write-txin-target (object)
  (let ((type (caar object))
        (target (cdar object)))
    (cond ((eq type :gen)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-gen-tag+)
                        (write-txin-gen target)))
          ((eq type :script)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-script-tag+)
                        (write-txin-to-script target)))
          ((eq type :scripthash)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-scripthash-tag+)
                        (write-txin-to-scripthash target)))
          ((eq type :key)
           (concatenate '(simple-array (unsigned-byte 8) (*))
                        (vector +txin-to-key-tag+)
                        (write-txin-to-key target))))))


;;; Signatures (before ring confidential transaction signatures)

(defun write-signatures (object)
  (write-custom-vector object #'write-custom-vector #'write-bytes))


;;; Ring confidential transaction signatures

(defun write-rct-sig-prunable (object)
  (labels ((write-key64 (object)
             (write-custom-vector object #'write-key))

           (write-boro-sig (object)
             (serialize object
                        ((s0 #'write-key64)
                         (s1 #'write-key64)
                         (ee #'write-key))))

           (write-range-sig (object)
             (serialize object
                        ((boro-sig #'write-boro-sig)
                         (ci #'write-key64))))

           (write-mg (object)
             (serialize object
                        ((ss #'write-custom-vector #'write-custom-vector #'write-key)
                         (cc #'write-key)))))
    (serialize object
               ((range-sigs #'write-custom-vector #'write-range-sig)
                (mgs #'write-custom-vector #'write-mg)))))

(defun write-rct-signatures (object)
  (flet ((write-pseudo-outputs (object type)
           (unless (eq type +rct-type-simple+)
             (write-custom-vector object #'write-key)))

         (write-ecdh-tuple (object)
           (serialize object
                      ((mask #'write-key)
                       (amount #'write-key)))))
    (let ((type (geta object :type)))
      (concatenate '(simple-array (unsigned-byte 8) (*))
                   (vector type)
                   (unless (eq type +rct-type-null+)
                     (serialize object
                                ((fee #'write-varint)
                                 (pseudo-outputs #'write-pseudo-outputs type)
                                 (ecdh-info #'write-custom-vector #'write-ecdh-tuple)
                                 (out-pk #'write-custom-vector #'write-key)
                                 (rct-sig-prunable #'write-rct-sig-prunable))))))))


;;; Transactions

(defun write-transaction-prefix (object)
  (serialize object
             ((version #'write-varint)
              (unlock-time #'write-varint)
              (inputs #'write-vector #'write-txin-target)
              (outputs #'write-vector #'write-txout)
              (extra #'write-byte-vector))))

(defun write-transaction (object)
  (let ((version (geta (geta object :prefix) :version)))
    (if (= 1 version)
        (serialize object
                   ((prefix #'write-transaction-prefix)
                    (signatures #'write-signatures)))
        (serialize object
                   ((prefix #'write-transaction-prefix)
                    (rct-signatures #'write-rct-signatures))))))


;;; Blocks

(defun write-block-header (object)
  (serialize object
             ((major-version #'write-varint)
              (minor-version #'write-varint)
              (timestamp #'write-varint)
              (previous-block-hash #'write-hash)
              (nonce #'write-bytes))))

(defun write-block (object)
  (serialize object
             ((header #'write-block-header)
              (miner-transaction #'write-transaction)
              (transaction-hashes #'write-vector #'write-hash))))
