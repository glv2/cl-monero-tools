;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun sign-message (message secret-spend-key)
  "Return a signature of a MESSAGE by a SECRET-SPEND-KEY."
  (let* ((header "SigV1")
         (hash (fast-hash (string->bytes message)))
         (signature-data (generate-signature hash secret-spend-key)))
    (concatenate 'string header (base58-encode signature-data))))

(defun valid-message-signature-p (message address signature)
  "Return T if a SIGNATURE of a MESSAGE by the secret key matching
an ADDRESS is valid, and NIL otherwise."
  (let* ((header "SigV1")
         (header-length (length header)))
  (unless (or (< (length signature) (+ header-length (* 2 +ed25519-key-length+))))
    (let ((hash (fast-hash (string->bytes message)))
          (public-key (geta (decode-address address) :public-spend-key))
          (signature-data (base58-decode (subseq signature header-length))))
      (valid-signature-p hash public-key signature-data)))))
