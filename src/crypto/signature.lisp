;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun generate-signature (data secret-key)
  "Return a Schnorr signature of DATA by SECRET-KEY."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (check-type secret-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (let* ((public-key (secret-key->public-key secret-key))
         (s (ironclad::ed25519-decode-int secret-key))
         (k-data (random-scalar))
         (k (ironclad::ed25519-decode-int k-data))
         (k-point (secret-key->public-key k-data))
         (c-data (hash-to-scalar (concatenate '(simple-array (unsigned-byte 8) (*))
                                              data public-key k-point)))
         (c (ironclad::ed25519-decode-int c-data))
         (r-data (ironclad::ed25519-encode-int (mod (- k (* c s)) ironclad::+ed25519-l+))))
    (concatenate '(simple-array (unsigned-byte 8) (*)) c-data r-data)))

(defun valid-signature-p (data public-key signature)
  "Return T if a Schnorr SIGNATURE of DATA by the secret key matching
a PUBLIC-KEY is valid, and NIL otherwise."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (check-type public-key (simple-array (unsigned-byte 8) (#.+ed25519-key-length+)))
  (check-type signature (simple-array (unsigned-byte 8) (#.(* 2 +ed25519-key-length+))))
  (let* ((g ironclad::+ed25519-b+)
         (p (ironclad::ed25519-decode-point public-key))
         (c (ironclad::ed25519-decode-int (subseq signature 0 +ed25519-key-length+)))
         (r (ironclad::ed25519-decode-int (subseq signature +ed25519-key-length+)))
         (k-point (ironclad::ed25519-edwards-add (ironclad::ed25519-scalar-mult g r)
                                                 (ironclad::ed25519-scalar-mult p c)))
         (k-data (ironclad::ed25519-encode-point k-point))
         (h-data (hash-to-scalar (concatenate '(simple-array (unsigned-byte 8) (*))
                                              data public-key k-data)))
         (h (ironclad::ed25519-decode-int h-data)))
    (= c h)))
