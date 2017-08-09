;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


;;; Blocks

(defconstant +block-tag+ 187)


;;; Transactions

(defconstant +transaction-tag+ 204)

;; Transaction inputs
(defconstant +transaction-input-generation-tag+ 255)
(defconstant +transaction-input-to-script-tag+ 0)
(defconstant +transaction-input-to-script-hash-tag+ 1)
(defconstant +transaction-input-to-key-tag+ 2)

;; Transaction outputs
(defconstant +transaction-output-to-script-tag+ 0)
(defconstant +transaction-output-to-script-hash-tag+ 1)
(defconstant +transaction-output-to-key-tag+ 2)

;; Ring confidential transaction signatures
(defconstant +rct-type-null+ 0)
(defconstant +rct-type-full+ 1)
(defconstant +rct-type-simple+ 2)
(defconstant +rct-key-tag+ 144)
(defconstant +rct-key64-tag+ 145)
(defconstant +rct-key-vector-tag+ 146)
(defconstant +rct-key-matrix-tag+ 147)
(defconstant +rct-ctkey-tag+ 148)
(defconstant +rct-ctkey-vector-tag+ 149)
(defconstant +rct-ctkey-matrix-tag+ 150)
(defconstant +rct-ecdh-tuple-tag+ 151)
(defconstant +rct-multilayered-group-signature-tag+ 152)
(defconstant +rct-range-proof-tag+ 153)
(defconstant +rct-boromean-signature-tag+ 154)
(defconstant +rct-rct-signature-tag+ 155)

;; Transaction extra data
(defconstant +transaction-extra-padding-tag+ 0)
(defconstant +transaction-extra-public-key-tag+ 1)
(defconstant +transaction-extra-nonce-tag+ 2)
(defconstant +transaction-extra-merge-mining-tag+ 3)
(defconstant +transaction-extra-nonce-payment-id-tag+ 0)
(defconstant +transaction-extra-nonce-encrypted-payment-id-tag+ 1)
(defconstant +transaction-extra-padding-max-size+ 254)
(defconstant +transaction-extra-nonce-max-size+ 254)
