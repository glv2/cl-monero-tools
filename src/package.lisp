;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools
  (:use :cl
        :alexandria
        :babel
        :bordeaux-threads
        :cffi
        :ieee-floats
        :octet-streams
        :split-sequence)
  (:export
   ;; blockchain
   #:acceptable-hash-p
   #:compute-block-hash #:compute-block-hash-from-data
   #:compute-miner-transaction-hash #:compute-miner-transaction-hash-from-data
   #:compute-transaction-hash #:compute-transaction-hash-from-data
   #:compute-transaction-tree-hash

   ;; crypto
   #:*cryptonight-variant*
   #:fast-hash #:slow-hash #:tree-hash
   #:chacha8 #:chacha20 #:generate-chacha-key
   #:generate-keys #:generate-secret-key
   #:secret-key->public-key #:secret-spend-key->secret-view-key
   #:recover-keys
   #:derive-key
   #:derive-output-public-key #:derive-output-secret-key
   #:derive-secret-spend-subkey #:derive-public-spend-subkey
   #:public-spend-subkey->public-view-subkey
   #:generate-signature #:valid-signature-p
   #:generate-ring-signature #:valid-ring-signature-p
   #:compute-key-image

   ;; mine
   #:miner
   #:mining-profitability

   ;; mnemonic
   #:available-mnemonic-seed-languages
   #:mnemonic-seed->secret-key #:secret-key->mnemonic-seed
   #:encrypt-mnemonic-seed #:decrypt-mnemonic-seed

   ;; openalias
   #:*dns-server*
   #:*dnssec-trust-anchor*
   #:get-openalias-info

   ;; serialization
   #:serialize-block #:deserialize-block
   #:serialize-block-header #:deserialize-block-header
   #:serialize-transaction #:deserialize-transaction
   #:serialize-transaction-prefix #:deserialize-transaction-prefix
   #:serialize-to-binary-storage #:deserialize-from-binary-storage

   ;; utils
   #:base58-encode #:base58-decode
   #:integer->bytes #:bytes->integer
   #:string->bytes #:bytes->string
   #:utf-8-string->bytes #:bytes->utf-8-string
   #:hex-string->bytes #:bytes->hex-string
   #:geta
   #:read-float #:format-float
   #:decode-json-from-string #:encode-json-to-string

   ;; wallet
   #:+monero-unit+
   #:decode-address #:make-integrated-address
   #:public-keys->address #:secret-spend-key->address
   #:public-keys->subaddress #:secret-spend-key->subaddress
   #:encrypt-payment-id #:decrypt-payment-id
   #:output-for-address-p
   #:decrypt-amount #:received-amount
   #:spent-key-images
   #:prove-payment #:valid-payment-proof-p
   #:prove-inbound-transaction #:valid-inbound-transaction-proof-p
   #:prove-outbound-transaction #:valid-outbound-transaction-proof-p
   #:get-wallet-keys #:bruteforce-wallet-keys
   #:sign-message #:valid-message-signature-p
   #:sign-file #:valid-file-signature-p
   #:make-uri #:decode-uri
   #:make-qr-code #:decode-qr-code))

(in-package :monero-tools)


(defconstant +monero-unit+ #.(expt 10 12))
(defconstant +block-time+ 120)
