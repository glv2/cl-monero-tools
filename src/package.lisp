;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools
  (:use :cl :alexandria :babel :base64 :bordeaux-threads :cffi :json :split-sequence)
  (:export
   ;; blockchain
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

   ;; mnemonic
   #:available-mnemonic-seed-languages
   #:mnemonic-seed->secret-key #:secret-key->mnemonic-seed

   ;; rpc
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:rpc #:json-rpc

   ;; serialization
   #:serialize-block #:deserialize-block
   #:serialize-transaction #:deserialize-transaction

   ;; utils
   #:base58-encode #:base58-decode
   #:bytes->hex-string #:bytes->integer
   #:hex-string->bytes #:integer->bytes #:string->bytes
   #:geta

   ;; wallet
   #:decode-address #:make-integrated-address
   #:public-keys->address #:secret-spend-key->address
   #:get-wallet-keys #:bruteforce-wallet-keys))
