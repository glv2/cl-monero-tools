;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun get-info-from-daemon (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (json-rpc "get_info"
            :host host
            :port port
            :user user
            :password password))

(defun zmq-get-info-from-daemon (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (zmq-json-rpc "get_info"
                :host host
                :port port
                :user user
                :password password))

(defun get-block-count-from-daemon (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (json-rpc "getblockcount"
                          :host host
                          :port port
                          :user user
                          :password password)))
    (geta answer :count)))

(defun zmq-get-block-count-from-daemon (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (zmq-json-rpc "get_height"
                              :host host
                              :port port
                              :user user
                              :password password)))
    (geta answer :height)))

(defun get-money-supply-from-daemon (start-height block-count &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (list (cons "height" start-height)
                          (cons "count" block-count))))
    (json-rpc "get_coinbase_tx_sum"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))

(defun get-transactions-from-daemon (transaction-ids &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons "txs_hashes" (coerce transaction-ids 'vector))
                           (cons "decode_as_json" t)))
         (answer (rpc "gettransactions"
                      :parameters parameters
                      :host host
                      :port port
                      :user user
                      :password password))
         (transactions (map 'list #'decode-json-from-string (geta answer :txs--as--json))))
    (map 'list (lambda (tx) (remove :rctsig--prunable tx :key #'car)) transactions)))

(defun zmq-get-transactions-from-daemon (transaction-ids &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons "tx_hashes" (coerce transaction-ids 'vector))))
         (answer (zmq-json-rpc "get_transactions"
                               :parameters parameters
                               :host host
                               :port port
                               :user user
                               :password password)))
    answer))

(defun get-transaction-data-from-daemon (transaction-ids &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons "txs_hashes" (coerce transaction-ids 'vector))))
         (answer (rpc "gettransactions"
                      :parameters parameters
                      :host host
                      :port port
                      :user user
                      :password password)))
    (map 'list
         (lambda (tx)
           (let ((data (geta tx :as--hex)))
             (when data
               (hex-string->bytes data))))
         (geta answer :txs))))

(defun get-block-from-daemon (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (list (cons (etypecase block-id
                                  (integer "height")
                                  (string "hash"))
                                block-id))))
    (json-rpc "getblock"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))

(defun zmq-get-block-from-daemon (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((get-by-height (integerp block-id))
         (parameters (list (cons (if get-by-height "height" "hash")
                                 block-id))))
    (zmq-json-rpc (if get-by-height "get_block_header_by_height" "get_block_header_by_hash")
                  :parameters parameters
                  :host host
                  :port port
                  :user user
                  :password password)))

(defun get-block-data-from-daemon (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons (etypecase block-id
                                   (integer "height")
                                   (string "hash"))
                                 block-id)))
         (answer (json-rpc "getblock"
                           :parameters parameters
                           :host host
                           :port port
                           :user user
                           :password password))
         (data (geta answer :blob)))
    (when data
      (hex-string->bytes data))))

(defun get-block-transaction-hashes-from-daemon (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((answer (get-block-from-daemon block-id
                                        :host host
                                        :port port
                                        :user user
                                        :password password))
         (block-data (hex-string->bytes (geta answer :blob)))
         (miner-transaction-hash (compute-miner-transaction-hash-from-data block-data))
         (regular-transaction-hashes (geta answer :tx--hashes)))
    (concatenate 'list (list miner-transaction-hash) regular-transaction-hashes)))

(defun get-block-template-from-daemon (address reserve-size &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (list (cons "wallet_address" address)
                          (cons "reserve_size" reserve-size))))
    (json-rpc "getblocktemplate"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))

(defun submit-block-to-daemon (block-data &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (list (bytes->hex-string block-data))))
    (json-rpc "submitblock"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))

(defun send-raw-transaction-to-daemon (transaction-data &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((parameters (list (cons "tx_as_hex" (bytes->hex-string transaction-data)))))
    (rpc "sendrawtransaction"
         :parameters parameters
         :host host
         :port port
         :user user
         :password password)))

(defun get-blocks-from-daemon (block-ids start-height prune &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((block-ids (bytes->string (apply #'concatenate 'octet-vector block-ids)))
         (parameters (list (cons :block--ids block-ids)
                           (cons :prune prune)
                           (cons :start--height (cons start-height '(unsigned-byte 64))))))
    (rpc "getblocks.bin"
         :binary t
         :parameters parameters
         :host host
         :port port
         :user user
         :password password)))

;; (get-blocks-from-daemon (list (hex-string->bytes "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148") (hex-string->bytes "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3")) 0 t)

(defun zmq-get-blocks-from-daemon (block-ids start-height prune &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((block-ids block-ids)
         (parameters (list (cons :block--ids block-ids)
                           (cons :prune prune)
                           (cons :start--height start-height))))
    (zmq-json-rpc "get_blocks_fast"
                  :parameters parameters
                  :host host
                  :port port
                  :user user
                  :password password)))

;; (zmq-get-blocks-from-daemon (list "5a1125384b088dbeaaa6f61c39db0318e53732ffc927978a52e3b16553203138" "48ca7cd3c8de5b6a4d53d2861fbdaedca141553559f9be9520068053cda8430b") 0 t) ; testnet

(defun get-hashes-from-daemon (block-ids start-height &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((block-ids (bytes->string (apply #'concatenate 'octet-vector block-ids)))
         (parameters (list (cons :block--ids block-ids)
                           (cons :start--height (cons start-height '(unsigned-byte 64)))))
         (answer (rpc "gethashes.bin"
                      :binary t
                      :parameters parameters
                      :host host
                      :port port
                      :user user
                      :password password)))
    (flet ((split (data-string)
             (let ((size (length data-string)))
               (unless (zerop (mod size +key-length+))
                 (error "Invalid length"))
               (map 'vector
                    #'string->bytes
                    (loop for i from 0 below size by +key-length+
                          collect (subseq data-string i (+ i +key-length+)))))))
      (setf (geta answer :m--block--ids) (split (geta answer :m--block--ids)))
      answer)))

;; (get-hashes-from-daemon (list (hex-string->bytes "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148") (hex-string->bytes "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3")) 0)

(defun get-blocks-by-height-from-daemon (block-heights &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((block-heights (make-array (length block-heights)
                                    :element-type '(unsigned-byte 64)
                                    :initial-contents block-heights))
         (parameters (list (cons :heights block-heights)))
         (answer (rpc "getblocks_by_height.bin"
                      :binary t
                      :parameters parameters
                      :host host
                      :port port
                      :user user
                      :password password))
         (blocks (geta answer :blocks)))
    (map 'vector (lambda (x) (string->bytes (geta x :block))) blocks)))

;; (get-blocks-by-height-from-daemon '(1000000))
