;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools-rpc
  (:use :cl
        :bordeaux-threads
        :base64
        :json
        :monero-tools
        :split-sequence)
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
   #:label-address))
