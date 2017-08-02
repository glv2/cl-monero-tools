;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


;;; Transaction outputs

(defconstant +txout-to-script-tag+ 0)
(defconstant +txout-to-scripthash-tag+ 1)
(defconstant +txout-to-key-tag+ 2)


;;; Transaction inputs

(defconstant +txin-gen-tag+ 255)
(defconstant +txin-to-script-tag+ 0)
(defconstant +txin-to-scripthash-tag+ 1)
(defconstant +txin-to-key-tag+ 2)


;;; Ring confidential transaction signatures

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
(defconstant +rct-mg-sig-tag+ 152)
(defconstant +rct-range-sig-tag+ 153)
(defconstant +rct-boro-sig-tag+ 154)
(defconstant +rct-rct-sig-tag+ 155)


;;; Transactions

(defconstant +transaction-tag+ 204)


;;; Blocks

(defconstant +block-tag+ 187)
