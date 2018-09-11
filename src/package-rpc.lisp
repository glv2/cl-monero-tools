;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools-rpc
  (:use :cl)
  (:import-from :bordeaux-threads
                #:join-thread
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from :base64
                #:usb8-array-to-base64-string)
  (:import-from :ironclad
                #:digest-sequence
                #:random-data)
  (:import-from :monero-tools
                #:*mine-lock*
                #:bytes->hex-string
                #:bytes->integer
                #:bytes->string
                #:compute-key-image
                #:compute-miner-transaction-hash-from-data
                #:decode-address
                #:decode-json-from-string
                #:decrypt-amount
                #:derive-key
                #:derive-output-public-key
                #:derive-output-secret-key
                #:derive-output-secret-subkey
                #:deserialize-block
                #:deserialize-from-binary-storage
                #:deserialize-transaction
                #:encode-json-to-string
                #:geta
                #:hex-string->bytes
                #:miner
                #:output-public-key->public-spend-subkey
                #:serialize-to-binary-storage
                #:spent-key-images
                #:string->bytes
                #:utf-8-string->bytes)
  (:import-from :split-sequence
                #:split-sequence)
  (:export
   ;; daemon
   #:flush-txpool
   #:get-alternate-chain
   #:get-bans
   #:get-block
   #:get-block-count
   #:get-block-hash
   #:get-block-header-by-hash
   #:get-block-header-by-height
   #:get-block-headers-range
   #:get-block-template
   #:get-coinbase-tx-sum
   #:get-connections
   #:get-fee-estimate
   #:get-info
   #:get-last-block-header
   #:get-output-distribution
   #:get-output-histogram
   #:get-txpool-backlog
   #:get-version
   #:hard-fork-info
   #:relay-tx
   #:set-bans
   #:submit-block
   #:sync-info

   #:get-alt-blocks-hashes
   #:get-blocks.bin
   #:get-blocks-by-height.bin
   #:get-hashes.bin
   #:get-limit
   #:get-o-indexes.bin
   #:get-outs
   #:get-outs.bin
   #:get-peer-list
   #:get-random-outs.bin
   #:get-random-rctouts.bin
   #:get-transaction-pool
   #:get-transaction-pool-hashes
   #:get-transaction-pool-hashes.bin
   #:get-transaction-pool-stats
   #:get-transactions
   #:in-peers
   #:is-key-image-spent
   #:mining-status
   #:out-peers
   #:save-bc
   #:set-limit
   #:set-log-categories
   #:set-log-hashrate
   #:set-log-level
   #:start-mining-daemon
   #:stop-daemon
   #:stop-mining-daemon
   #:update

   #:zmq-get-block
   #:zmq-get-info
   #:zmq-get-transactions

   ;; history
   #:transaction-history

   ;; mine
   #:mine-block

   ;; rpc
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:rpc #:json-rpc
   #:zmq-json-rpc

   ;; wallet
   #:add-address-book
   #:create-account
   #:create-address
   #:create-wallet
   #:delete-address-book
   #:export-key-images
   #:get-account-tags
   #:get-accounts
   #:get-address
   #:get-address-book
   #:get-balance
   #:get-bulk-payments
   #:get-height
   #:get-languages
   #:get-payments
   #:get-transfer-by-txid
   #:get-transfers
   #:get-tx-notes
   #:import-key-images
   #:incoming_transfers
   #:label-account
   #:label-address
   #:make-integrated-address
   #:make-uri
   #:open-wallet
   #:parse-uri
   #:query-key
   #:rescan-blockchain
   #:rescan-spent
   #:set-account-tag-description
   #:set-tx-notes
   #:sign
   #:split-integrated-address
   #:start-mining
   #:stop-mining
   #:stop-wallet
   #:store
   #:sweep-all
   #:sweep-dust
   #:tag-accounts
   #:transfer
   #:transfer-split
   #:untag-accounts
   #:verify))
