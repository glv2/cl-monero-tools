;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +multisig-info-header+ "MultisigV1" :test #'string=)
(define-constant +multisig-extra-info-header+ "MultisigxV1" :test #'string=)

(defun encode-multisig-info (secret-view-key secret-spend-key)
  "Return the info of a multi-signature wallet encoded as a string."
  (let* ((blinded-secret-view-key (compute-multisig-blinded-secret secret-view-key))
         (secret-signer-key (compute-multisig-blinded-secret secret-spend-key))
         (public-signer-key (secret-key->public-key secret-signer-key))
         (data (concatenate 'octet-vector blinded-secret-view-key public-signer-key))
         (hash (fast-hash data))
         (signature (generate-signature hash secret-signer-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-info-header+ (base58-encode data))))

(defun decode-multisig-info (multisig-info)
  "Return an alist containing the components of the MULTISIG-INFO string."
  (let ((header-length (length +multisig-info-header+))
        (encoded-data-length (base58-encoded-length (* 4 +key-length+))))
    (if (and (= (length multisig-info) (+ header-length encoded-data-length))
             (string= multisig-info +multisig-info-header+ :end1 header-length))
        (let* ((data (base58-decode (subseq multisig-info header-length)))
               (secret-view-key (subseq data 0 +key-length+))
               (public-signer-key (subseq data +key-length+ (* 2 +key-length+)))
               (hash (fast-hash (subseq data 0 (* 2 +key-length+))))
               (signature (subseq data (* 2 +key-length+))))
          (if (valid-signature-p hash public-signer-key signature)
              (list (cons :secret-view-key secret-view-key)
                    (cons :public-signer-key public-signer-key))
              (error "Invalid multisig info.")))
        (error "Invalid multisig info."))))

(defun encode-multisig-extra-info (multisig-keys secret-spend-key)
  "Return the extra info of a multi-signature wallet encoded as a string."
  (let* ((secret-signer-key (compute-multisig-blinded-secret secret-spend-key))
         (public-signer-key (secret-key->public-key secret-signer-key))
         (multisig-public-keys (compute-multisig-public-keys multisig-keys))
         (data (apply #'concatenate 'octet-vector public-signer-key multisig-public-keys))
         (hash (fast-hash data))
         (signature (generate-signature hash secret-signer-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-extra-info-header+ (base58-encode data))))

(defun decode-multisig-extra-info (multisig-extra-info)
  "Return an alist containing the components of the MULTISIG-EXTRA-INFO string."
  (let ((header-length (length +multisig-extra-info-header+))
        (min-encoded-data-length (base58-encoded-length (* 4 +key-length+))))
    (if (and (>= (length multisig-extra-info) (+ header-length min-encoded-data-length))
             (string= multisig-extra-info +multisig-extra-info-header+ :end1 header-length))
        (let* ((data (base58-decode (subseq multisig-extra-info header-length)))
               (public-signer-key (subseq data 0 +key-length+))
               (keys-data-length (- (length data) (* 3 +key-length+)))
               (multisig-public-keys-data (subseq data +key-length+ (+ +key-length+ keys-data-length)))
               (hash (fast-hash (subseq data 0 (+ +key-length+ keys-data-length))))
               (signature (subseq data (+ +key-length+ keys-data-length))))
          (if (and (zerop (mod keys-data-length +key-length+))
                   (valid-signature-p hash public-signer-key signature))
              (let ((keys (loop for i from 0 below keys-data-length by +key-length+
                                collect (subseq multisig-public-keys-data i (+ i +key-length+)))))
                (list (cons :public-signer-key public-signer-key)
                      (cons :multisig-public-keys (coerce keys 'vector))))
              (error "Invalid multisig extra info.")))
        (error "Invalid multisig extra info."))))
