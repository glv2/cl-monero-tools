;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
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
               "ieee-floats"
               "ironclad"
               "png-read"
               "split-sequence")
  :in-order-to ((test-op (test-op "monero-tools/tests")))
  :components ((:module "src"
                :components ((:file "package")
                             (:module "blockchain"
                              :depends-on ("crypto" "package" "serialization" "utils")
                              :components ((:file "block" :depends-on ("transaction"))
                                           (:file "transaction")))
                             (:module "crypto"
                              :depends-on ("package" "utils")
                              :components ((:file "blake")
                                           (:file "crypto" :depends-on ("cryptonight" "keccak"))
                                           (:file "cryptonight" :depends-on ("blake" "keccak" "pseudo-aes"))
                                           (:file "keccak")
                                           (:file "key" :depends-on ("crypto"))
                                           (:file "proof" :depends-on ("crypto" "key"))
                                           (:file "pseudo-aes")
                                           (:file "signature" :depends-on ("crypto" "key"))))
                             (:module "mine"
                              :depends-on ("blockchain" "package")
                              :components ((:file "miner")
                                           (:file "profitability")))
                             (:module "mnemonic"
                              :depends-on ("package" "utils")
                              :components ((:file "chinese-simplified" :depends-on ("mnemonic"))
                                           (:file "dutch" :depends-on ("mnemonic"))
                                           (:file "english" :depends-on ("mnemonic"))
                                           (:file "esperanto" :depends-on ("mnemonic"))
                                           (:file "french" :depends-on  ("mnemonic"))
                                           (:file "german" :depends-on ("mnemonic"))
                                           (:file "italian" :depends-on ("mnemonic"))
                                           (:file "japanese" :depends-on ("mnemonic"))
                                           (:file "lojban" :depends-on ("mnemonic"))
                                           (:file "mnemonic")
                                           (:file "portuguese" :depends-on ("mnemonic"))
                                           (:file "russian" :depends-on ("mnemonic"))
                                           (:file "spanish" :depends-on ("mnemonic"))))
                             (:module "openalias"
                              :depends-on ("package" "utils")
                              :components ((:file "dns")
                                           (:file "openalias" :depends-on ("dns"))))
                             (:module "serialization"
                              :depends-on ("crypto" "package" "utils")
                              :components ((:file "constants")
                                           (:file "deserialization" :depends-on ("constants"))
                                           (:file "serialization" :depends-on ("constants"))
                                           (:file "storage" :depends-on ("constants"))))
                             (:module "utils"
                              :depends-on ("package")
                              :components ((:file "base58" :depends-on ("utils"))
                                           (:file "utils")))
                             (:module "wallet"
                              :depends-on ("crypto" "package" "serialization" "utils")
                              :components ((:file "address")
                                           (:file "qr" :depends-on ("uri"))
                                           (:file "signature" :depends-on ("address"))
                                           (:file "transaction" :depends-on ("address"))
                                           (:file "uri")
                                           (:file "wallet-file")))))))

(defsystem "monero-tools/tests"
  :name "monero-tools/tests"
  :description "Tests for monero-tools"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("fiveam"
               "monero-tools"
               "uiop")
  :in-order-to ((test-op (load-op "monero-tools/tests")))
  :perform (test-op (op s)
                    (let ((tests (uiop:find-symbol* 'monero-tools-tests :monero-tools/tests)))
                      (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :components ((:file "blockchain" :depends-on ("tests"))
                             (:file "crypto" :depends-on ("tests"))
                             (:file "mine" :depends-on ("tests"))
                             (:file "mnemonic" :depends-on ("tests"))
                             (:file "openalias" :depends-on ("tests"))
                             (:file "tests")
                             (:file "serialization" :depends-on ("tests"))
                             (:file "utils" :depends-on ("tests"))
                             (:file "wallet" :depends-on ("tests"))))))
