;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun make-handshake-request (&key (network-id +p2p-network-id-mainnet+) (peer-id (random-data 8)) (my-port 0))
  (let ((payload (list (cons "node_data"
                             (list (cons "local_time" (get-unix-time)) ; TODO: serialization to uint64?
                                   (cons "my_port" my-port) ; TODO: serialization to uint32?
                                   (cons "network_id" (coerce network-id 'octet-vector))
                                   (cons "peer_id" (coerce peer-id 'octet-vector))))
                       (cons "payload_data"
                             (list (cons "cumulative_difficulty" 1)
                                   (cons "current_height" 1)
                                   (cons "top_id" +mainnet-genesis-hash+)
                                   (cons "top_version" 1))))))
    (make-request +p2p-command-handshake+
                  (serialize-to-binary-storage payload))))
