;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-daemon-rpc)


;;; Specs in https://getmonero.org/resources/developer-guides/daemon-rpc.html


;;; HTTP JSON RPCs

(defjsonrpc flush-txpool ("flush_txpool" &key transaction-ids)
  "Flush transaction ids from transaction pool."
  (when transaction-ids
    (list (cons "txids" (coerce transaction-ids 'vector)))))

(defjsonrpc generateblocks ("generateblocks" amount-of-blocks wallet-address previous-block &key starting-nonce)
  "Generate blocs."
  (append (list (cons "amount_of_blocks" amount-of-blocks)
                (cons "wallet_address" wallet-address)
                (cons "prev_block" previous-block))
          (when starting-nonce
            (list (cons "starting_nonce" starting-nonce)))))

(defjsonrpc get-alternate-chains ("get_alternate_chains")
  "Get alternative chains seen by the node.")

(defjsonrpc get-bans ("get_bans")
  "Get list of banned IPs.")

(defjsonrpc get-block ("get_block" block-id &key fill-pow-hash)
  "Get full block information. BLOCK-ID can be a block height or hash."
  (append (list (cons (etypecase block-id
                        (integer "height")
                        (string "hash"))
                      block-id))
          (when fill-pow-hash
            (list (cons "fill_pow_hash" fill-pow-hash)))))

(defjsonrpc get-block-count ("get_block_count")
  "Look up how many blocks are in the longest chain known to the node.")

(defjsonrpc get-block-hash ("on_get_block_hash" block-height)
  "Look up a block's hash by its height."
  (vector block-height))

(defjsonrpc get-block-header-by-hash ("get_block_header_by_hash" block-hash &key fill-pow-hash)
  "Look up a block's header by its hash."
  (append (list (cons "hash" block-hash))
          (when fill-pow-hash
            (list (cons "fill_pow_hash" fill-pow-hash)))))

(defjsonrpc get-block-header-by-height ("get_block_header_by_height" block-height &key fill-pow-hash)
  "Look up a block's header by its height."
  (append (list (cons "height" block-height))
          (when fill-pow-hash
            (list (cons "fill_pow_hash" fill-pow-hash)))))

(defjsonrpc get-block-headers-range ("get_block_headers_range" start-height end-height &key fill-pow-hash)
  "Look up headers of a range of blocks."
  (append (list (cons "start_height" start-height)
                (cons "end_height" end-height))
          (when fill-pow-hash
            (list (cons "fill_pow_hash" fill-pow-hash)))))

(defjsonrpc get-block-template ("get_block_template" address reserve-size &key previous-block)
  "Get a block template on which mining can be done."
  (append (list (cons "wallet_address" address)
                (cons "reserve_size" reserve-size))
          (when previous-block
            (list (cons "prev_block" previous-block)))))

(defjsonrpc get-coinbase-tx-sum ("get_coinbase_tx_sum" start-height block-count)
  "Get the amount of money emitted and fees in the BLOCK-COUNT blocks starting
at START-HEIGHT."
  (list (cons "height" start-height)
        (cons "count" block-count)))

(defjsonrpc get-connections ("get_connections")
  "Retrieve information about incoming and outgoing connections to your node.")

(defjsonrpc get-fee-estimate ("get_fee_estimate" &key grace-blocks)
  "Gives an estimation on fees per kB."
  (when grace-blocks
    (list (cons "grace_blocks" grace-blocks))))

(defjsonrpc get-info ("get_info")
  "Retrieve general information about the state of your node and the network.")

(defjsonrpc get-last-block-header ("get_last_block_header" &key fill-pow-hash)
  "Look up the block header of the most recent block."
  (when fill-pow-hash
    (list (cons "fill_pow_hash" fill-pow-hash))))

(defjsonrpc get-output-distribution ("get_output_distribution" amounts &key cumulative start-height end-height binary compress)
  "Get output distribution."
  (append (list (cons "amounts" (coerce amounts 'vector)))
          (when cumulative
            (list (cons "cumulative" t)))
          (when start-height
            (list (cons "from_height" start-height)))
          (when end-height
            (list (cons "to_height" end-height)))
          (when binary
            (list (cons "binary" t)))
          (when compress
            (list (cons "compress" t)))))

(defjsonrpc get-output-histogram ("get_output_histogram" amounts &key min-count max-count unlocked recent-cutoff)
  "Get a histogram of output amounts."
  (append (list (cons "amounts" (coerce amounts 'vector)))
          (when min-count
            (list (cons "min_count" min-count)))
          (when max-count
            (list (cons "max_count" max-count)))
          (when unlocked
            (list (cons "unlocked" unlocked)))
          (when recent-cutoff
            (list (cons "recent_cutoff" recent-cutoff)))))

(defjsonrpc get-txpool-backlog ("get_txpool_backlog")
  "Get all transaction pool backlog.")

(defjsonrpc get-version ("get_version")
  "Get the node current version.")

(defjsonrpc hard-fork-info ("hard_fork_info" &key version)
  "Look up information regarding hard fork voting and readiness."
  (when version
    (list (cons "version" version))))

(defjsonrpc prune-blockchain ("prune_blockchain" &key check)
  "Prune the blockchain."
  (when check
    (list (cons "check" t))))

(defjsonrpc relay-tx ("relay_tx" transaction-ids)
  "Relay a list of transaction IDs."
  (list (cons "txids" (coerce transaction-ids 'vector))))

(defjsonrpc set-bans ("set_bans" bans)
  "Ban some nodes."
  (list (cons "bans" (coerce bans 'vector))))

(defjsonrpc submit-block ("submit_block" block-data)
  "Submit a mined block to the network."
  (vector block-data))

(defjsonrpc sync-info ("sync_info")
  "Get synchronisation information.")


;;; Other HTTP RPCs

(defrpc get-alt-blocks-hashes ("get_alt_blocks_hashes")
  "Get the known blocks hashes which are not on the main chain.")

(defrpc get-height ("get_height")
  "Get blockchain height.")

(defrpc get-limit ("get_limit")
  "Get daemon bandwidth limits.")

(defrpc get-net-stats ("get_net_stats")
  "Get network stastitics.")

(defrpc get-outs ("get_outs" outputs &key (get-transaction-id t))
  "Get outputs."
  (list (cons "outputs" (coerce outputs 'vector))
        (cons "get_txid" (when get-transaction-id t))))

(defrpc get-peer-list ("get_peer_list")
  "Get the known peers list.")

(defrpc get-public-nodes ("get_public_nodes" &key gray (white t))
  "Get public nodes list."
  (list (cons "gray" (when gray t))
        (cons "white" (when white t))))

(defrawrpc get-transaction-pool ("get_transaction_pool")
  "Show information about valid transactions seen by the node but not yet mined
  into a block, as well as spent key image information for the txpool in the
  node's memory."
  nil
  (lambda (result)
    (let* ((result (let ((json:*use-strict-json-rules* nil))
                     (decode-json-from-string (bytes->string result))))
           (transactions (geta result :transactions)))
      (dolist (transaction transactions)
        (setf (geta transaction :tx-blob) (string->bytes (geta transaction :tx-blob))))
      result)))

(defrpc get-transaction-pool-hashes ("get_transaction_pool_hashes")
  "Get hashes from transaction pool.")

(defrawrpc get-transaction-pool-stats ("get_transaction_pool_stats")
  "Get the transaction pool statistics."
  nil
  (lambda (result)
    (let* ((result (let ((json:*use-strict-json-rules* nil))
                     (decode-json-from-string (bytes->string result))))
           (stats (geta result :pool-stats))
           (histo (when stats
                    (geta stats :histo))))
      (when histo
        (setf (geta stats :histo) (string->bytes histo)))
      result)))

(defrpc get-transactions ("get_transactions" transaction-ids &key decode-as-json prune split)
  "Look up one or more transactions by hash."
  (append (list (cons "txs_hashes" (coerce transaction-ids 'vector)))
          (when decode-as-json
            (list (cons "decode_as_json" t)))
          (when prune
            (list (cons "prune" t)))
          (when split
            (list (cons "split" t)))))

(defrpc in-peers ("in_peers" limit)
  "Limit the number of incoming connections from peers."
  (list (cons "in_peers" limit)))

(defrpc is-key-image-spent ("is_key_image_spent" key-images)
  "Check if outputs have been spent using the key image associated with the output."
  (list (cons "key_images" (coerce key-images 'vector))))

(defrpc mining-status ("mining_status")
  "Get the mining status of the daemon.")

(defrpc out-peers ("out_peers" limit)
  "Limit the number of outgoing connections to peers."
  (list (cons "out_peers" limit)))

(defrpc pop-blocks ("pop_blocks" block-count)
  "Remove blocks from the top of the blockchain."
  (list (cons "nblocks" block-count)))

(defrpc save-bc ("save_bc")
  "Save the blockchain.")

(defrpc send-raw-transaction ("send_raw_transaction" transaction &key do-not-relay (do-sanity-checks t))
  "Broadcast a raw transaction to the network."
  (append (list (cons "tx_as_hex" transaction))
          (when do-not-relay
            (list (cons "do_not_relay" t)))
          (list (cons "do_sanity_checks" do-sanity-checks))))

(defrpc set-bootstrap-daemon ("set_bootstrap_daemon" address &key username password)
  "Set the bootstrap daemon to use."
  (append (list (cons "address" address))
          (when username
            (list (cons "username" username)))
          (when password
            (list (cons "password" password)))))

(defrpc set-limit ("set_limit" download-limit upload-limit)
  "Set daemon bandwidth limits (kB/s)."
  (list (cons "limit_down" download-limit)
        (cons "limit_up" upload-limit)))

(defrpc set-log-categories ("set_log_categories" &key categories)
  "Set the daemon log categories."
  (when categories
    (list (cons "categories" categories))))

(defrpc set-log-hash-rate ("set_log_hash_rate" visible)
  "Set the log hash rate display mode."
  (when visible
    (list (cons "visible" (when visible t))
          ;; workaround to prevent (("visible" . nil)) from being encoded
          ;; as [["visible"]] instead of {"visible":false}
          (cons "unused" 0))))

(defrpc set-log-level ("set_log_level" level)
  "Set the daemon log level."
  (list (cons "level" level)))

(defrpc start-mining ("start_mining" miner-address thread-count background-mining ignore-battery)
  "Start mining in the daemon."
  (list (cons "miner_address" miner-address)
        (cons "threads_count" thread-count)
        (cons "do_background_mining" (when background-mining t))
        (cons "ignore_battery" (when ignore-battery t))))

(defrpc start-save-graph ("start_save_graph")
  "Start saving graph.")

(defrpc stop-daemon ("stop_daemon")
  "Send a command to the daemon to safely disconnect and shut down.")

(defrpc stop-mining ("stop_mining")
  "Stop mining in the daemon.")

(defrpc stop-save-graph ("stop_save_graph")
  "Stop saving graph.")

(defrpc update ("update" command &key path)
  "Update the daemon."
  (append (list (cons "command" command))
          (when path
            (list (cons "path" path)))))
