;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools
  (:use :cl :alexandria :babel :base64 :bordeaux-threads :cffi :json :split-sequence)
  (:export #:*rpc-host*
           #:*rpc-port*
           #:*rpc-user*
           #:*rpc-password*
           #:available-mnemonic-seed-languages
           #:base58-encode
           #:base58-decode
           #:bruteforce-wallet-keys
           #:bytes->hex-string
           #:bytes->integer
           #:chacha8
           #:decode-address
           #:fast-hash
           #:generate-chacha8-key
           #:generate-keys
           #:generate-secret-key
           #:get-wallet-keys
           #:geta
           #:hex-string->bytes
           #:integer->bytes
           #:json-rpc
           #:make-integrated-address
           #:mnemonic-seed->secret-key
           #:public-keys->address
           #:recover-keys
           #:rpc
           #:secret-key->mnemonic-seed
           #:secret-key->public-key
           #:secret-spend-key->address
           #:secret-spend-key->secret-view-key
           #:slow-hash
           #:string->bytes))
