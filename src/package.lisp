;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools
  (:use :cl :alexandria :babel :base64 :bordeaux-threads :cffi :ieee-floats :json :split-sequence)
  (:export
   ;; blockchain
   #:acceptable-hash-p
   #:compute-block-hash #:compute-block-hash-from-data
   #:compute-miner-transaction-hash #:compute-miner-transaction-hash-from-data
   #:compute-transaction-hash #:compute-transaction-hash-from-data
   #:compute-transaction-tree-hash

   ;; crypto
   #:fast-hash #:slow-hash #:tree-hash
   #:chacha8 #:generate-chacha8-key
   #:generate-keys #:generate-secret-key
   #:secret-key->public-key #:secret-spend-key->secret-view-key
   #:recover-keys
   #:derive-key
   #:generate-signature #:valid-signature-p
   #:generate-ring-signature #:valid-ring-signature-p
   #:compute-key-image

   ;; mine
   #:miner

   ;; mnemonic
   #:available-mnemonic-seed-languages
   #:mnemonic-seed->secret-key #:secret-key->mnemonic-seed
   #:encrypt-mnemonic-seed #:decrypt-mnemonic-seed

   ;; rpc
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:rpc #:json-rpc

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

   ;; wallet
   #:decode-address #:make-integrated-address
   #:public-keys->address #:secret-spend-key->address
   #:encrypt-payment-id #:decrypt-payment-id
   #:output-for-address-p
   #:get-wallet-keys #:bruteforce-wallet-keys
   #:sign-message #:valid-message-signature-p
   #:sign-file #:valid-file-signature-p))
