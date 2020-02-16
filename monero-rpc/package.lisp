;;;; This file is part of monero-tools
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-rpc
  (:use :cl :monero-utils)
  (:import-from :base64
                #:usb8-array-to-base64-string)
  (:import-from :ironclad
                #:digest-sequence
                #:random-data)
  (:import-from :split-sequence
                #:split-sequence)
  (:export
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:parse-digest-authentication-challenge
   #:compute-digest-authentication-response
   #:rpc #:json-rpc
   #:defrpc #:defrawrpc #:defjsonrpc))

(defpackage :monero-daemon-rpc
  (:use :cl :monero-rpc :monero-utils)
  (:export
   #:flush-txpool
   #:generateblocks
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
   #:prune-blockchain
   #:relay-tx
   #:set-bans
   #:submit-block
   #:sync-info

   #:get-alt-blocks-hashes
   #:get-height
   #:get-limit
   #:get-net-stats
   #:get-outs
   #:get-peer-list
   #:get-public-nodes
   #:get-transaction-pool
   #:get-transaction-pool-hashes
   #:get-transaction-pool-stats
   #:get-transactions
   #:in-peers
   #:is-key-image-spent
   #:mining-status
   #:out-peers
   #:pop-blocks
   #:save-bc
   #:send-raw-transaction
   #:set-bootstrap-daemon
   #:set-limit
   #:set-log-categories
   #:set-log-hashrate
   #:set-log-level
   #:start-mining
   #:start-save-graph
   #:stop-daemon
   #:stop-mining
   #:stop-save-graph
   #:update))

(defpackage :monero-wallet-rpc
  (:use :cl :monero-rpc :monero-utils)
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
   #:set-log-categories
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
