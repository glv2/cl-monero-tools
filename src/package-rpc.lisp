;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools-rpc
  (:use :cl)
  (:import-from :base64
                #:usb8-array-to-base64-string)
  (:import-from :ironclad
                #:digest-sequence
                #:random-data)
  (:import-from :monero-tools
                #:bytes->hex-string
                #:decode-json-from-string
                #:deserialize-from-binary-storage
                #:encode-json-to-string
                #:geta
                #:serialize-to-binary-storage
                #:string->bytes
                #:utf-8-string->bytes)
  (:import-from :split-sequence
                #:split-sequence)
  (:export
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:rpc #:json-rpc
   #:defrpc #:defbinrpc #:defrawrpc #:defjsonrpc
   #:zmq-json-rpc))

(defpackage :monero-tools-daemon-rpc
  (:use :cl :monero-tools-rpc)
  (:import-from :bordeaux-threads
                #:join-thread
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from :monero-tools
                #:*mine-lock*
                #:bytes->hex-string
                #:bytes->integer
                #:bytes->string
                #:compute-key-image
                #:compute-miner-transaction-hash-from-data
                #:decode-json-from-string
                #:decrypt-amount
                #:derive-key
                #:derive-output-public-key
                #:derive-output-secret-key
                #:derive-output-secret-subkey
                #:deserialize-block
                #:deserialize-transaction
                #:geta
                #:hex-string->bytes
                #:miner
                #:output-public-key->public-spend-subkey
                #:spent-key-images
                #:string->bytes)
  (:export
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
   #:start-mining
   #:stop-daemon
   #:stop-mining
   #:update

   #:zmq-get-block
   #:zmq-get-info
   #:zmq-get-transactions

   ;; history
   #:transaction-history

   ;; mine
   #:mine-block))

(defpackage :monero-tools-wallet-rpc
  (:use :cl :monero-tools-rpc)
  (:export
   #:add-address-book
   #:auto-refresh
   #:change-wallet-password
   #:check-reserve-proof
   #:check-spend-proof
   #:check-tx-key
   #:check-tx-proof
   #:close-wallet
   #:create-account
   #:create-address
   #:create-wallet
   #:delete-address-book
   #:describe-transfer
   #:exchange-multisig-keys
   #:export-key-images
   #:export-multisig-info
   #:export-outputs
   #:finalize-multisig
   #:generate-from-keys
   #:get-account-tags
   #:get-accounts
   #:get-address
   #:get-address-book
   #:get-address-index
   #:get-attribute
   #:get-balance
   #:get-bulk-payments
   #:get-height
   #:get-languages
   #:get-payments
   #:get-reserve-proof
   #:get-spend-proof
   #:get-transfer-by-txid
   #:get-transfers
   #:get-tx-key
   #:get-tx-notes
   #:get-tx-proof
   #:get-version
   #:import-key-images
   #:import-multisig-info
   #:import-outputs
   #:incoming_transfers
   #:is-multisig
   #:label-account
   #:label-address
   #:make-integrated-address
   #:make-multisig
   #:make-uri
   #:open-wallet
   #:parse-uri
   #:prepare-multisig
   #:query-key
   #:refresh
   #:relay-tx
   #:rescan-blockchain
   #:rescan-spent
   #:restore-deterministic-wallet
   #:set-account-tag-description
   #:set-attribute
   #:set-daemon
   #:set-log-level
   #:set-tx-notes
   #:sign
   #:sign-multisig
   #:sign-transfer
   #:split-integrated-address
   #:start-mining
   #:stop-mining
   #:stop-wallet
   #:store
   #:submit-multisig
   #:submit-transfer
   #:sweep-all
   #:sweep-dust
   #:sweep-single
   #:tag-accounts
   #:transfer
   #:transfer-split
   #:untag-accounts
   #:validate-address
   #:verify))
