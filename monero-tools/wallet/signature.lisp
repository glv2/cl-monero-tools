;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +message-signature-header+ "SigV1" :test #'string=)

(defun sign-message (message secret-spend-key)
  "Return a signature of a MESSAGE by a SECRET-SPEND-KEY."
  (let* ((hash (fast-hash (utf-8-string->bytes message)))
         (signature-data (generate-signature hash secret-spend-key)))
    (concatenate 'string +message-signature-header+ (base58-encode signature-data))))

(defun valid-message-signature-p (message address signature)
  "Return T if a SIGNATURE of a MESSAGE by the secret key matching
an ADDRESS is valid, and NIL otherwise."
  (let ((header-length (length +message-signature-header+))
        (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length signature) (+ header-length encoded-signature-length))
               (string= signature +message-signature-header+ :end1 header-length))
      (let ((hash (fast-hash (utf-8-string->bytes message)))
            (public-key (geta (decode-address address) :public-spend-key))
            (signature-data (base58-decode (subseq signature header-length))))
        (valid-signature-p hash public-key signature-data)))))

(defun sign-file (file secret-spend-key)
  "Return a signature of a FILE by a SECRET-SPEND-KEY."
  (let* ((hash (fast-hash (read-file-into-byte-vector file)))
         (signature-data (generate-signature hash secret-spend-key)))
    (concatenate 'string +message-signature-header+ (base58-encode signature-data))))

(defun valid-file-signature-p (file address signature)
  "Return T if a SIGNATURE of a FILE by the secret key matching
an ADDRESS is valid, and NIL otherwise."
  (let ((header-length (length +message-signature-header+))
        (encoded-signature-length (base58-encoded-length (* 2 +key-length+))))
    (when (and (= (length signature) (+ header-length encoded-signature-length))
               (string= signature +message-signature-header+ :end1 header-length))
      (let ((hash (fast-hash (read-file-into-byte-vector file)))
            (public-key (geta (decode-address address) :public-spend-key))
            (signature-data (base58-decode (subseq signature header-length))))
        (valid-signature-p hash public-key signature-data)))))
