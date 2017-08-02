;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun server-get-info (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (json-rpc "get_info"
            :host host
            :port port
            :user user
            :password password))

(defun server-get-block-count (&key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let ((answer (json-rpc "getblockcount"
                          :host host
                          :port port
                          :user user
                          :password password)))
    (geta answer :count)))

(defun server-get-transactions (transaction-ids &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
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

(defun server-get-transaction-data (transaction-ids &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons "txs_hashes" (coerce transaction-ids 'vector))))
         (answer (rpc "gettransactions"
                      :parameters parameters
                      :host host
                      :port port
                      :user user
                      :password password))
         (transactions (map 'list (lambda (tx) (geta tx :as--hex)) (geta answer :txs))))
    transactions))

(defun server-get-block (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons (etypecase block-id
                                   (integer "height")
                                   (string "hash"))
                                 block-id))))
    (json-rpc "getblock"
              :parameters parameters
              :host host
              :port port
              :user user
              :password password)))

(defun server-get-block-data (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((parameters (list (cons (etypecase block-id
                                   (integer "height")
                                   (string "hash"))
                                 block-id)))
         (answer (json-rpc "getblock"
                           :parameters parameters
                           :host host
                           :port port
                           :user user
                           :password password)))
    (geta answer :blob)))

(defun server-get-block-transaction-hashes (block-id &key (host *rpc-host*) (port *rpc-port*) (user *rpc-user*) (password *rpc-password*))
  (let* ((block-data (hex-string->bytes (server-get-block-data block-id
                                                               :host host
                                                               :port port
                                                               :user user
                                                               :password password)))
         (miner-transaction-hash (compute-miner-transaction-hash block-data))
         (regular-transaction-hashes (geta (read-block block-data 0) :tx-hashes)))
    (concatenate 'list (list miner-transaction-hash) regular-transaction-hashes)))
