;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-tools"
  :name "monero-tools"
  :description "Tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("alexandria"
               "babel"
               "bordeaux-threads"
               "cffi"
               "cl-json"
               "cl-octet-streams"
               "cl-ppcre"
               "cl-qrencode"
               "dexador"
               "ieee-floats"
               "ironclad"
               "iterate"
               "monero-utils"
               "png-read"
               "split-sequence")
  :in-order-to ((test-op (test-op "monero-tools-tests")))
  :components ((:module "monero-tools"
                :serial t
                :components ((:file "package")
                             (:module "crypto"
                              :serial t
                              :components ((:file "asm-sbcl-x86-64")
                                           (:file "blake")
                                           (:file "keccak")
                                           (:file "pseudo-aes")
                                           (:file "random-math")
                                           (:file "cryptonight")
                                           (:file "randomx-service")
                                           (:file "randomx")
                                           (:file "crypto")
                                           (:file "key")
                                           (:file "multisig")
                                           (:file "proof")
                                           (:file "signature")))
                             (:module "mnemonic"
                              :serial t
                              :components ((:file "mnemonic")
                                           (:file "chinese-simplified")
                                           (:file "dutch")
                                           (:file "english")
                                           (:file "esperanto")
                                           (:file "french")
                                           (:file "german")
                                           (:file "italian")
                                           (:file "japanese")
                                           (:file "lojban")
                                           (:file "portuguese")
                                           (:file "russian")
                                           (:file "spanish")))
                             (:module "openalias"
                              :serial t
                              :components ((:file "dns")
                                           (:file "openalias")))
                             (:module "serialization"
                              :serial t
                              :components ((:file "constants")
                                           (:file "deserialization")
                                           (:file "serialization")
                                           (:file "storage")))
                             (:module "blockchain"
                              :serial t
                              :components ((:file "transaction")
                                           (:file "block")))
                             (:module "mine"
                              :components ((:file "miner")
                                           (:file "profitability")))
                             (:module "wallet"
                              :serial t
                              :components ((:file "address")
                                           (:file "multisig")
                                           (:file "uri")
                                           (:file "qr")
                                           (:file "signature")
                                           (:file "transaction")
                                           (:file "wallet-file")))))))
