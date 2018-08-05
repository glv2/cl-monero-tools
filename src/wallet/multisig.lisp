;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +multisig-info-header+ "MultisigV1" :test #'string=)
(define-constant +multisig-extra-info-header+ "MultisigxV1" :test #'string=)
(define-constant +multisig-signature-header+ "SigMultisigPkV1" :test #'string=)

(defun encode-multisig-info (secret-view-key secret-spend-key)
  (let* ((blinded-secret-view-key (compute-multisig-blinded-secret secret-view-key))
         (signer-secret-key (compute-multisig-blinded-secret secret-spend-key))
         (signer-public-key (secret-key->public-key signer-secret-key))
         (data (concatenate 'octet-vector blinded-secret-view-key signer-public-key))
         (hash (fast-hash data))
         (signature (generate-signature hash signer-secret-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-info-header+ (base58-encode data))))

(defun decode-multisig-info (multisig-info)
  ;; TODO
  )

(defun encode-multisig-extra-info (multisig-keys secret-spend-key)
  (let* ((signer-secret-key (compute-multisig-blinded-secret secret-spend-key))
         (signer-public-key (secret-key->public-key signer-secret-key))
         (multisig-public-keys (compute-multisig-public-keys multisig-keys))
         (data (apply #'concatenate 'octet-vector signer-public-key multisig-public-keys))
         (hash (fast-hash data))
         (signature (generate-signature hash signer-secret-key))
         (data (concatenate 'octet-vector data signature)))
    (concatenate 'string +multisig-extra-info-header+ (base58-encode data))))

(defun decode-multisig-extra-info (multisig-extra-info)
  ;; TODO
  )
