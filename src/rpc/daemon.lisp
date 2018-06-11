;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-rpc)


;;; Specs in https://getmonero.org/resources/developer-guides/daemon-rpc.html


;;; HTTP JSON RPCs

(defjsonrpc flush-txpool ("flush_txpool" &key transaction-ids)
  "Flush transaction ids from transaction pool."
  (when transaction-ids
    (list (cons "txids" transaction-ids))))

(defjsonrpc get-alternate-chains ("get_alternate_chains")
  "Get alternative chains seen by the node.")

(defjsonrpc get-bans ("get_bans")
  "Get list of banned IPs.")

(defjsonrpc get-block ("get_block" block-id)
  "Get full block information. BLOCK-ID can be a block height or hash."
  (list (cons (etypecase block-id
                (integer "height")
                (string "hash"))
              block-id)))

(defjsonrpc get-block-count ("get_block_count")
  "Look up how many blocks are in the longest chain known to the node.")

(defjsonrpc get-block-hash ("on_get_block_hash" block-height)
  "Look up a block's hash by its height."
  (vector block-height))

(defjsonrpc get-block-header-by-hash ("get_block_header_by_hash" block-hash)
  "Look up a block's header by its hash."
  (list (cons "hash" block-hash)))

(defjsonrpc get-block-header-by-height ("get_block_header_by_height" block-height)
  "Look up a block's header by its height."
  (list (cons "height" block-height)))

(defjsonrpc get-block-headers-range ("get_block_headers_range" start-height end-height)
  "Look up headers of a range of blocks."
  (list (cons "start_height" start-height)
        (cons "end_height" end-height)))

(defjsonrpc get-block-template ("get_block_template" address reserve-size)
  "Get a block template on which mining can be done."
  (list (cons "wallet_address" address)
        (cons "reserve_size" reserve-size)))

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

(defjsonrpc get-last-block-header ("get_last_block_header")
  "Look up the block header of the most recent block.")

(defjsonrpc get-output-distribution ("get_output_distribution" amounts &key cumulative start-height end-height)
  "Get output distribution."
  (append (list (cons "amounts" amounts))
          (when cumulative
            (list (cons "cumulative" t)))
          (when start-height
            (list (cons "from_height" start-height)))
          (when end-height
            (list (cons "to_height" end-height)))))

(defjsonrpc get-output-histogram ("get_output_histogram" amounts &key min-count max-count unlocked recent-cutoff)
  "Get a histogram of output amounts."
  (append (list (cons "amounts" amounts))
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

(defjsonrpc hard-fork-info ("hard_fork_info")
  "Look up information regarding hard fork voting and readiness.")

(defjsonrpc relay-tx ("relay_tx" transaction-ids)
  "Relay a list of transaction IDs."
  (list (cons "txids" transaction-ids)))

(defjsonrpc set-bans ("set_bans" bans)
  "Ban some nodes."
  (list (cons "bans" bans)))

(defjsonrpc submit-block ("submit_block" block-data)
  "Submit a mined block to the network."
  (list block-data))

(defjsonrpc sync-info ("sync_info")
  "Get synchronisation information.")


;;; Other HTTP RPCs

(defrpc get-alt-blocks-hashes ("get_alt_blocks_hashes")
  "Get the known blocks hashes which are not on the main chain.")

(defbinrpc get-blocks.bin ("get_blocks.bin" block-ids &key start-height prune)
  "Get all blocks info."
  (let ((hex-block-ids (with-output-to-string (s)
                         (dolist (id (coerce block-ids 'list))
                           (write-string id s)))))
    (append (list (cons "block_ids" (bytes->string (hex-string->bytes hex-block-ids))))
            (when start-height
              (list (cons "start_height" start-height)))
            (when prune
              (list (cons "prune" t)))))
  (lambda (result)
    (let ((blocks (geta result :blocks)))
      (dotimes (i (length blocks))
        (let ((b (aref blocks i)))
          (setf (geta b :block) (string->bytes (geta b :block))))))
    result))

;; (get-blocks.bin (list "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148" "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3"))

(defbinrpc get-blocks-by-height.bin ("get_blocks_by_height.bin" heights)
  "Get blocks by height."
  (list (cons "heights" (coerce heights '(simple-array (unsigned-byte 64) (*)))))
  (lambda (result)
    (let ((blocks (geta result :blocks)))
      (dotimes (i (length blocks))
        (let ((b (aref blocks i)))
          (setf (geta b :block) (string->bytes (geta b :block))))))
    result))

(defbinrpc get-hashes.bin ("get_hashes.bin" block-ids &key start-height)
  "Get hashes."
  (let ((hex-block-ids (with-output-to-string (s)
                         (dolist (id (coerce block-ids 'list))
                           (write-string id s)))))
    (append (list (cons "block_ids" (bytes->string (hex-string->bytes hex-block-ids))))
            (when start-height
              (list (cons "start_height" start-height)))))
  (lambda (result)
    (let* ((data-string (geta result :m-block-ids))
           (size (length data-string))
           (key-length 32))
      (unless (zerop (mod size key-length))
        (error "Invalid length"))
      (setf (geta result :m-block-ids)
            (map 'vector
                 #'string->bytes
                 (loop for i from 0 below size by key-length
                       collect (subseq data-string i (+ i key-length))))))
    result))

;; (get-hashes.bin (list "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148" "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3"))

(defrpc get-height ("get_height")
  "Get the node's current height.")

(defrpc get-limit ("get_limit")
  "Get daemon bandwidth limits.")

(defbinrpc get-o-indexes.bin ("get_o_indexes.bin" transaction-id)
  "Get global output indexes of a transaction."
  (list (cons "txid" (bytes->string (hex-string->bytes transaction-id)))))

(defrpc get-outs ("get_outs" outputs)
  "Get outputs."
  (list (cons "outputs" outputs)))

(defbinrpc get-outs.bin ("get_outs.bin" outputs)
  "Get outputs."
  (list (cons "outputs" outputs))
  (lambda (result)
    (let ((outs (geta result :outs)))
      (dotimes (i (length outs))
        (let ((out (aref outs i)))
          (setf (geta out :key) (string->bytes (geta out :key)))
          (setf (geta out :mask) (string->bytes (geta out :mask)))
          (setf (geta out :txid) (string->bytes (geta out :txid))))))
    result))

(defrpc get-peer-list ("get_peer_list")
  "Get the known peers list.")

(defbinrpc get-random-outs.bin ("get_random_outs.bin" amounts output-count)
  "Get a list of random outputs for a specific list of amounts."
  (list (cons "amounts" amounts)
        (cons "outs_count" output-count))
  (lambda (result)
    (let ((outs (geta result :outs)))
      (dotimes (i (length outs))
        (let* ((x (aref outs i))
               (y (string->bytes (geta x :outs)))
               (z (loop for j from 0 below (length y) by 40
                        collect (list (cons :amount-index (bytes->integer y :start j :end (+ j 8)))
                                      (cons :output-key (subseq y (+ j 8) (+ j 40)))))))
          (setf (geta x :outs) z))))
    result))

(defbinrpc get-random-rctouts.bin ("get_random_rctouts.bin" output-count)
  "Get random RingCT outputs."
  (list (cons "outs_count" output-count))
  (lambda (result)
    (let* ((outs (geta result :outs))
           (x (string->bytes outs))
           (y (loop for i from 0 below (length x) by 80
                    collect (list (cons :amount (bytes->integer x :start i :end (+ i 8)))
                                  (cons :amount-index (bytes->integer x :start (+ i 8) :end (+ i 16)))
                                  (cons :output-key (subseq x (+ i 16) (+ i 48)))
                                  (cons :commitment (subseq x (+ i 48) (+ i 80)))))))
      (setf (geta result :outs) y))
    result))

#|
    /get_transaction_pool
    /get_transaction_pool_hashes.bin
    /get_transaction_pool_stats
    /get_transactions
    /in_peers
    /is_key_image_spent
    /mining_status
    /out_peers
    /save_bc
    /send_raw_transaction
    /set_limit
    /set_log_categories
    /set_log_hash_rate
    /set_log_level
    /start_mining
    /start_save_graph
    /stop_daemon
    /stop_mining
    /stop_save_graph
    /update
|#


(defun get-transactions-from-daemon (transaction-ids &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((parameters (list (cons "txs_hashes" (coerce transaction-ids 'vector))
                           (cons "decode_as_json" t)))
         (answer (rpc "gettransactions"
                      :parameters parameters
                      :rpc-host rpc-host
                      :rpc-port rpc-port
                      :rpc-user rpc-user
                      :rpc-password rpc-password))
         (transactions (map 'list #'decode-json-from-string (geta answer :txs-as-json))))
    (map 'list (lambda (tx) (remove :rctsig-prunable tx :key #'car)) transactions)))

(defun get-transaction-data-from-daemon (transaction-ids &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((parameters (list (cons "txs_hashes" (map 'vector #'bytes->hex-string transaction-ids))))
         (answer (rpc "gettransactions"
                      :parameters parameters
                      :rpc-host rpc-host
                      :rpc-port rpc-port
                      :rpc-user rpc-user
                      :rpc-password rpc-password)))
    (map 'list
         (lambda (tx)
           (let ((data (geta tx :as-hex)))
             (when data
               (hex-string->bytes data))))
         (geta answer :txs))))

(defun get-block-data-from-daemon (block-id &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((parameters (list (cons (etypecase block-id
                                   (integer "height")
                                   (string "hash"))
                                 block-id)))
         (answer (json-rpc "getblock"
                           :parameters parameters
                           :rpc-host rpc-host
                           :rpc-port rpc-port
                           :rpc-user rpc-user
                           :rpc-password rpc-password))
         (data (geta answer :blob)))
    (when data
      (hex-string->bytes data))))

(defun get-block-transaction-hashes-from-daemon (block-id &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((answer (get-block block-id
                            :rpc-host rpc-host
                            :rpc-port rpc-port
                            :rpc-user rpc-user
                            :rpc-password rpc-password))
         (block-data (hex-string->bytes (geta answer :blob)))
         (miner-transaction-hash (compute-miner-transaction-hash-from-data block-data))
         (regular-transaction-hashes (geta answer :tx-hashes)))
    (cons (bytes->hex-string miner-transaction-hash) regular-transaction-hashes)))

(defun send-raw-transaction-to-daemon (transaction-data &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let ((parameters (list (cons "tx_as_hex" (bytes->hex-string transaction-data)))))
    (rpc "sendrawtransaction"
         :parameters parameters
         :rpc-host rpc-host
         :rpc-port rpc-port
         :rpc-user rpc-user
         :rpc-password rpc-password)))


;;; ZeroMQ RPCs

(defun zmq-get-info-from-daemon (&key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (zmq-json-rpc "get_info"
                :rpc-host rpc-host
                :rpc-port rpc-port
                :rpc-user rpc-user
                :rpc-password rpc-password))

(defun zmq-get-block-count-from-daemon (&key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let ((answer (zmq-json-rpc "get_height"
                              :rpc-host rpc-host
                              :rpc-port rpc-port
                              :rpc-user rpc-user
                              :rpc-password rpc-password)))
    (geta answer :height)))

(defun zmq-get-transactions-from-daemon (transaction-ids &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((parameters (list (cons "tx_hashes" (coerce transaction-ids 'vector))))
         (answer (zmq-json-rpc "get_transactions"
                               :parameters parameters
                               :rpc-host rpc-host
                               :rpc-port rpc-port
                               :rpc-user rpc-user
                               :rpc-password rpc-password)))
    answer))

(defun zmq-get-block-from-daemon (block-id &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((get-by-height (integerp block-id))
         (parameters (list (cons (if get-by-height "height" "hash")
                                 block-id))))
    (zmq-json-rpc (if get-by-height "get_block_header_by_height" "get_block_header_by_hash")
                  :parameters parameters
                  :rpc-host rpc-host
                  :rpc-port rpc-port
                  :rpc-user rpc-user
                  :rpc-password rpc-password)))

(defun zmq-get-blocks-from-daemon (block-ids start-height prune &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((block-ids block-ids)
         (parameters (list (cons "block_ids" block-ids)
                           (cons "prune" prune)
                           (cons "start_height" start-height))))
    (zmq-json-rpc "get_blocks_fast"
                  :parameters parameters
                  :rpc-host rpc-host
                  :rpc-port rpc-port
                  :rpc-user rpc-user
                  :rpc-password rpc-password)))

;; (zmq-get-blocks-from-daemon (list "5a1125384b088dbeaaa6f61c39db0318e53732ffc927978a52e3b16553203138" "48ca7cd3c8de5b6a4d53d2861fbdaedca141553559f9be9520068053cda8430b") 0 t) ; testnet
