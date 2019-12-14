;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-zmq-rpc)


(defun get-info (&key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (zmq-json-rpc "get_info"
                :rpc-host rpc-host
                :rpc-port rpc-port
                :rpc-user rpc-user
                :rpc-password rpc-password))

(defun get-transactions (transaction-ids &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let ((parameters (list (cons "tx_hashes" (coerce transaction-ids 'vector)))))
    (zmq-json-rpc "get_transactions"
                  :parameters parameters
                  :rpc-host rpc-host
                  :rpc-port rpc-port
                  :rpc-user rpc-user
                  :rpc-password rpc-password)))

(defun get-block (block-id &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((get-by-height (integerp block-id))
         (parameters (list (cons (if get-by-height "height" "hash")
                                 block-id))))
    (zmq-json-rpc (if get-by-height "get_block_header_by_height" "get_block_header_by_hash")
                  :parameters parameters
                  :rpc-host rpc-host
                  :rpc-port rpc-port
                  :rpc-user rpc-user
                  :rpc-password rpc-password)))

(defun get-blocks (block-ids start-height prune &key (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  (let* ((block-ids block-ids)
         (parameters (list (cons "block_ids" block-ids)
                           (cons "prune" (when prune t))
                           (cons "start_height" start-height))))
    (zmq-json-rpc "get_blocks_fast"
                  :parameters parameters
                  :rpc-host rpc-host
                  :rpc-port rpc-port
                  :rpc-user rpc-user
                  :rpc-password rpc-password)))

;; (zmq-get-blocks (list "5a1125384b088dbeaaa6f61c39db0318e53732ffc927978a52e3b16553203138" "48ca7cd3c8de5b6a4d53d2861fbdaedca141553559f9be9520068053cda8430b") 0 t) ; testnet
