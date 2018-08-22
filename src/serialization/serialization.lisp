;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defmacro serialize (stream object specs)
  (loop with result = (gensym)
        with id = (gensym)
        for spec in specs
        for name = (car spec)
        for writer = (cadr spec)
        for writer-parameters = (cddr spec)
        collect `(let ((,id ,(intern (string-upcase (symbol-name name)) :keyword)))
                   (apply ,writer ,result (geta ,object ,id) (list ,@writer-parameters)))
          into forms
        finally (return `(let ((,result (or ,stream (make-octet-output-stream))))
                           ,@forms
                           (unless ,stream
                             (prog1 (get-output-stream-octets ,result)
                               (close ,result)))))))

(defmacro serialize-variant (stream object specs)
  (loop with result = (gensym)
        with type = (gensym)
        with thing = (gensym)
        for spec in specs
        for id = (intern (string-upcase (symbol-name (car spec))) :keyword)
        for tag = (cadr spec)
        for writer = (caddr spec)
        for writer-parameters = (cdddr spec)
        collect `((eq ,type ,id)
                  (write-byte ,tag ,result)
                  (apply ,writer ,result ,thing (list ,@writer-parameters)))
          into forms
        finally (return `(let ((,result (or ,stream (make-octet-output-stream)))
                               (,type (caar ,object))
                               (,thing (cdar object)))
                           (cond ,@forms)
                           (unless ,stream
                             (prog1 (get-output-stream-octets ,result)
                               (close ,result)))))))


;;; Basic types

(defun serialize-single-byte (stream object)
  (if stream
      (write-byte object stream)
      (make-array 1 :element-type '(unsigned-byte 8) :initial-element object)))

(defun serialize-bytes (stream object)
  (if stream
      (write-sequence object stream)
      object))

(defun serialize-integer (stream object)
  (let ((bytes (integer->bytes object :varint t)))
    (if stream
        (write-sequence bytes stream)
        bytes)))

(defun serialize-vector (stream objects element-writer &rest element-writer-parameters)
  (let ((size (length objects))
        (result (or stream (make-octet-output-stream))))
    (serialize-integer result size)
    (dotimes (i size)
      (apply element-writer result (aref objects i) element-writer-parameters))
    (unless stream
      (prog1 (get-output-stream-octets result)
        (close result)))))

(defun serialize-custom-vector (stream objects element-writer &rest element-writer-parameters)
  (let ((size (length objects))
        (result (or stream (make-octet-output-stream))))
    (dotimes (i size)
      (apply element-writer result (aref objects i) element-writer-parameters))
    (unless stream
      (prog1 (get-output-stream-octets result)
        (close result)))))

(defun serialize-byte-vector (stream object)
  (if stream
      (progn
        (serialize-integer stream (length object))
        (write-sequence object stream))
      (concatenate 'octet-vector (serialize-integer nil (length object)) object)))

(defun serialize-key (stream object)
  (serialize-bytes stream object))

(defun serialize-hash (stream object)
  (serialize-bytes stream object))


;;; Transaction outputs

(defun serialize-transaction-output-to-script (stream object)
  (serialize stream object
    ((keys #'serialize-vector #'serialize-key)
     (script #'serialize-byte-vector))))

(defun serialize-transaction-output-to-script-hash (stream object)
  (serialize-hash stream object))

(defun serialize-transaction-output-to-key (stream object)
  (serialize-key stream object))

(defun serialize-transaction-output-target (stream object)
  (serialize-variant stream object
    ((script +transaction-output-to-script-tag+
             #'serialize-transaction-output-to-script)
     (script-hash +transaction-output-to-script-hash-tag+
                  #'serialize-transaction-output-to-script-hash)
     (key +transaction-output-to-key-tag+
          #'serialize-transaction-output-to-key))))

(defun serialize-transaction-output (stream object)
  (serialize stream object
    ((amount #'serialize-integer)
     (target #'serialize-transaction-output-target))))


;;; Transaction inputs

(defun serialize-transaction-input-generation (stream object)
  (serialize stream object
    ((height #'serialize-integer))))

(defun serialize-transaction-input-to-script (stream object)
  (serialize stream object
    ((prev #'serialize-hash)
     (prevout #'serialize-integer)
     (sigset #'serialize-byte-vector))))

(defun serialize-transaction-input-to-script-hash (stream object)
  (serialize stream object
    ((prev #'serialize-hash)
     (prevout #'serialize-integer)
     (script #'serialize-transaction-output-to-script)
     (sigset #'serialize-byte-vector))))

(defun serialize-transaction-input-to-key (stream object)
  (serialize stream object
    ((amount #'serialize-integer)
     (key-offsets #'serialize-vector #'serialize-integer)
     (key-image #'serialize-bytes))))

(defun serialize-transaction-input-target (stream object)
  (serialize-variant stream object
    ((generation +transaction-input-generation-tag+
                 #'serialize-transaction-input-generation)
     (script +transaction-input-to-script-tag+
             #'serialize-transaction-input-to-script)
     (script-hash +transaction-input-to-script-hash-tag+
                  #'serialize-transaction-input-to-script-hash)
     (key +transaction-input-to-key-tag+
          #'serialize-transaction-input-to-key))))


;;; Signatures (before ring confidential transaction signatures)

(defun serialize-signature (stream object)
  (serialize-custom-vector stream object #'serialize-custom-vector #'serialize-bytes))


;;; Ring confidential transaction signatures

(defun serialize-rct-range-proof (stream object)
  (labels ((serialize-key64 (stream object)
             (serialize-custom-vector stream object #'serialize-key))

           (serialize-boromean-signature (stream object)
             (serialize stream object
               ((s0 #'serialize-key64)
                (s1 #'serialize-key64)
                (ee #'serialize-key))))

           (serialize-range-proof (stream object)
             (serialize stream object
               ((boromean-signature #'serialize-boromean-signature)
                (pedersen-commitments #'serialize-key64))))

           (serialize-multilayered-group-signature (stream object)
             (serialize stream object
               ((ss #'serialize-custom-vector #'serialize-custom-vector #'serialize-key)
                (cc #'serialize-key)))))
    (when object
      (serialize stream object
        ((range-proofs #'serialize-custom-vector #'serialize-range-proof)
         (multilayered-group-signatures #'serialize-custom-vector
                                        #'serialize-multilayered-group-signature))))))

(defun serialize-rct-bulletproof (stream object type)
  (flet ((serialize-bulletproof (stream object)
           (serialize stream object
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

         (serialize-multilayered-group-signature (stream object)
           (serialize stream object
             ((ss #'serialize-custom-vector #'serialize-custom-vector #'serialize-key)
              (cc #'serialize-key))))

         (serialize-pseudo-outputs (stream object type)
           (when (= type +rct-type-simple-bulletproof+)
             (serialize-custom-vector stream object #'serialize-key))))
    (when object
      (serialize stream object
        ((bulletproofs #'serialize-custom-vector #'serialize-bulletproof)
         (multilayered-group-signatures #'serialize-custom-vector
                                        #'serialize-multilayered-group-signature)
         (pseudo-outputs #'serialize-pseudo-outputs type))))))

(defun serialize-rct-signature (stream object)
  (flet ((serialize-pseudo-outputs (stream object type)
           (when (= type +rct-type-simple+)
             (serialize-custom-vector stream object #'serialize-key)))

         (serialize-ecdh-tuple (stream object)
           (serialize stream object
             ((mask #'serialize-key)
              (amount #'serialize-key)))))
    (let ((type (geta object :type))
          (result (or stream (make-octet-output-stream))))
      (write-byte type result)
      (ecase type
        ((#.+rct-type-null+)
         nil)

        ((#.+rct-type-full+ #.+rct-type-simple+)
         (serialize result object
           ((fee #'serialize-integer)
            (pseudo-outputs #'serialize-pseudo-outputs type)
            (ecdh-info #'serialize-custom-vector #'serialize-ecdh-tuple)
            (output-public-keys #'serialize-custom-vector #'serialize-key)
            (rct-signature-prunable #'serialize-rct-range-proof))))

        ((#.+rct-type-full-bulletproof+ #.+rct-type-simple-bulletproof+)
         (serialize result object
           ((fee #'serialize-integer)
            (ecdh-info #'serialize-custom-vector #'serialize-ecdh-tuple)
            (output-public-keys #'serialize-custom-vector #'serialize-key)
            (rct-signature-prunable #'serialize-rct-bulletproof type)))))
      (unless stream
        (prog1 (get-output-stream-octets result)
          (close result))))))


;;; Transaction extra data

(defun serialize-transaction-extra-nonce (stream object)
  (let ((bytes (with-octet-output-stream (result)
                 (let ((type (caar object))
                       (data (cdar object)))
                   (when (> (length data) +transaction-extra-nonce-max-size+)
                     (error "Too much data in nonce"))
                   (case type
                     ((:payment-id)
                      (write-byte +transaction-extra-nonce-payment-id-tag+ result))
                     ((:encrypted-payment-id)
                      (write-byte +transaction-extra-nonce-encrypted-payment-id-tag+ result)))
                   (serialize-bytes result data))))
        (result (or stream (make-octet-output-stream))))
    (serialize-byte-vector result bytes)
    (unless stream
      (prog1 (get-output-stream-octets result)
        (close result)))))

(defun serialize-transaction-extra-data-field (stream object)
  (let ((result (serialize-variant nil object
                  ((padding +transaction-extra-padding-tag+
                            (lambda (stream data)
                              (if (> (length data) +transaction-extra-padding-max-size+)
                                  (error "Too much data in padding.")
                                  (serialize-bytes stream data))))
                   (transaction-public-key +transaction-extra-public-key-tag+
                                           #'serialize-key)
                   (additional-public-keys +transaction-extra-additional-public-keys-tag+
                                           #'serialize-vector #'serialize-key)
                   (nonce +transaction-extra-nonce-tag+
                          #'serialize-transaction-extra-nonce)))))
    (cond
      ((zerop (length result)) (serialize stream object
                                 ((data #'serialize-bytes))))
      (stream (write-sequence result stream))
      (t result))))

(defun serialize-transaction-extra-data (stream object)
  (serialize-byte-vector stream (with-octet-output-stream (result)
                                  (dolist (field object)
                                    (serialize-transaction-extra-data-field result field)))))


;;; Transactions

(defun serialize-transaction-prefix (stream object)
  "Return a transaction prefix OBJECT as a byte vector."
  (serialize stream object
    ((version #'serialize-integer)
     (unlock-time #'serialize-integer)
     (inputs #'serialize-vector #'serialize-transaction-input-target)
     (outputs #'serialize-vector #'serialize-transaction-output)
     (extra #'serialize-transaction-extra-data))))

(defun serialize-transaction (stream object)
  "Return a transaction OBJECT as a byte vector."
  (let ((version (geta (geta object :prefix) :version)))
    (case version
      ((1) (serialize stream object
             ((prefix #'serialize-transaction-prefix)
              (signature #'serialize-signature))))
      ((2) (serialize stream object
             ((prefix #'serialize-transaction-prefix)
              (rct-signature #'serialize-rct-signature))))
      (t (error "Transaction version ~d not supported." version)))))


;;; Blocks

(defun serialize-block-header (stream object)
  "Return a block header OBJECT as a byte vector."
  (serialize stream object
    ((major-version #'serialize-integer)
     (minor-version #'serialize-integer)
     (timestamp #'serialize-integer)
     (previous-block-hash #'serialize-hash)
     (nonce #'serialize-bytes))))

(defun serialize-block (stream object)
  "Return a block OBJECT as a byte vector."
  (serialize stream object
    ((header #'serialize-block-header)
     (miner-transaction #'serialize-transaction)
     (transaction-hashes #'serialize-vector #'serialize-hash))))
