;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools
  (:use :cl :iterate :monero-utils)
  (:import-from :alexandria
                #:define-constant
                #:hash-table-keys
                #:read-file-into-byte-vector)
  (:import-from :bordeaux-threads
                #:join-thread
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from :cffi
                #:defcstruct
                #:define-foreign-library
                #:foreign-funcall
                #:foreign-library-loaded-p
                #:inc-pointer
                #:load-foreign-library-error
                #:mem-aref
                #:mem-ref
                #:null-pointer
                #:null-pointer-p
                #:pointer-eq
                #:use-foreign-library
                #:with-foreign-object
                #:with-foreign-slots)
  (:import-from :ironclad
                #:digest-sequence
                #:encrypt
                #:expt-mod
                #:integer-to-octets
                #:make-cipher
                #:make-digest
                #:octets-to-integer
                #:produce-digest
                #:random-data
                #:strong-random
                #:ub32ref/be
                #:ub32ref/le
                #:ub64ref/le
                #:update-digest
                ;; Internals
                #:+ed25519-b+
                #:+ed25519-i+
                #:+ed25519-l+
                #:+ed25519-q+
                #:+ed25519-point-at-infinity+
                #:aes-round-keys
                #:allocate-round-keys
                #:copy-block
                #:dotimes-unrolled
                #:ec-add
                #:ec-decode-point
                #:ec-encode-point
                #:ec-point-equal
                #:ec-scalar-inv
                #:ec-scalar-mult
                #:ed25519-point
                #:first-byte
                #:fourth-byte
                #:generate-round-keys-for-encryption
                #:mod32+
                #:mod32-
                #:mod32*
                #:mod64+
                #:mod64-
                #:mod64*
                #:mod64ash
                #:mod64lognot
                #:rol32
                #:rol64
                #:ror32
                #:second-byte
                #:store-words
                #:Te0
                #:Te1
                #:Te2
                #:Te3
                #:third-byte
                #:with-words
                #:xor-block)
  (:import-from :octet-streams
                #:get-output-stream-octets
                #:make-octet-output-stream
                #:with-octet-input-stream
                #:with-octet-output-stream)
  (:import-from :split-sequence
                #:split-sequence)
  (:export
   ;; blockchain
   #:acceptable-hash-p
   #:compute-block-hash #:compute-block-hash-from-data
   #:compute-miner-transaction-hash #:compute-miner-transaction-hash-from-data
   #:compute-transaction-hash #:compute-transaction-hash-from-data
   #:compute-transaction-tree-hash

   ;; crypto
   #:*slow-hash-variant* #:*slow-hash-height* #:*slow-hash-seed*
   #:*randomx-service-host* #:*randomx-service-port*
   #:fast-hash #:slow-hash #:tree-hash
   #:chacha8 #:chacha20 #:generate-chacha-key
   #:generate-keys #:generate-secret-key
   #:secret-key->public-key #:secret-spend-key->secret-view-key
   #:recover-keys
   #:derive-key
   #:derive-output-public-key #:derive-output-secret-key
   #:derive-subkey-secret #:derive-output-secret-subkey
   #:derive-secret-spend-subkey #:derive-public-spend-subkey
   #:output-public-key->public-spend-subkey
   #:public-spend-subkey->public-view-subkey
   #:compute-subaddress-indexes-table
   #:generate-signature #:valid-signature-p
   #:generate-ring-signature #:valid-ring-signature-p
   #:compute-key-image
   #:compute-multisig-blinded-secret
   #:compute-multisig-secret-view-key
   #:compute-multisig-keys-n/n
   #:compute-multisig-keys-m/n
   #:compute-multisig-secret-spend-key
   #:compute-multisig-public-keys
   #:compute-multisig-public-spend-key
   #:randomx-service-info
   #:randomx-service-reseed
   #:randomx-service-hash
   #:randomx-service-hash-batch
   #:randomx-seed-height

   ;; mine
   #:modify-block-template
   #:miner
   #:mining-profitability

   ;; mnemonic
   #:available-mnemonic-seed-languages
   #:mnemonic-seed->secret-key #:secret-key->mnemonic-seed
   #:encrypt-mnemonic-seed #:decrypt-mnemonic-seed

   ;; openalias
   #:*dns-server*
   #:*dnssec-trust-anchors*
   #:get-openalias-info

   ;; serialization
   #:serialize-block #:deserialize-block
   #:serialize-block-header #:deserialize-block-header
   #:serialize-transaction #:deserialize-transaction
   #:serialize-transaction-prefix #:deserialize-transaction-prefix
   #:serialize-to-binary-storage #:deserialize-from-binary-storage

   ;; wallet
   #:+monero-unit+
   #:decode-address #:make-integrated-address
   #:public-keys->address #:secret-spend-key->address
   #:public-keys->subaddress #:secret-spend-key->subaddress
   #:valid-address-p
   #:encrypt-payment-id #:decrypt-payment-id
   #:output-for-address-p #:output-destination-address
   #:decrypt-amount #:received-amount
   #:spent-key-images
   #:prove-payment #:valid-payment-proof-p
   #:prove-inbound-transaction #:valid-inbound-transaction-proof-p
   #:prove-outbound-transaction #:valid-outbound-transaction-proof-p
   #:decrypt-wallet-keys
   #:get-wallet-keys #:bruteforce-wallet-keys
   #:sign-message #:valid-message-signature-p
   #:sign-file #:valid-file-signature-p
   #:make-uri #:decode-uri
   #:make-qr-code #:decode-qr-code
   #:make-multisig-info #:decode-multisig-info
   #:make-multisig-extra-info #:decode-multisig-extra-info
   #:make-multisig-seed #:decode-multisig-seed))

(in-package :monero-tools)


(defconstant +monero-unit+ #.(expt 10 12))
(defconstant +block-time+ 120)
