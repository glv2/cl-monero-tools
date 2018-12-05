;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun make-handshake-request (&key (network-id +p2p-network-id-mainnet+) (peer-id (random-bits 64)) (my-port 0))
  (let* ((network-id (coerce network-id 'octet-vector))
         (node-data (list (cons "local_time" (cons (get-unix-time) '(unsigned-byte 64)))
                          (cons "my_port" (cons my-port '(unsigned-byte 32)))
                          (cons "network_id" (bytes->string network-id))
                          (cons "peer_id" (cons peer-id '(unsigned-byte 64)))))
         (payload-data (list (cons "cumulative_difficulty" (cons 1 '(unsigned-byte 64)))
                             (cons "current_height" (cons 1 '(unsigned-byte 64)))
                             (cons "top_id" (bytes->string +mainnet-genesis-hash+))
                             (cons "top_version" (cons 1 '(unsigned-byte 8)))))
         (payload (list (cons "node_data" node-data)
                        (cons "payload_data" payload-data))))
    (make-request +p2p-command-handshake+
                  (serialize-to-binary-storage payload))))

(defun make-flags-response ()
  (let ((payload (list (cons "support_flags" (cons +p2p-support-flags+ '(unsigned-byte 8))))))
    (make-response +p2p-command-request-support-flags+
                   (serialize-to-binary-storage payload)
                   0)))

(defun open-connection (host port)
  (usocket:socket-connect host port
                          :protocol :stream
                          :element-type '(unsigned-byte 8)))

(defun close-connection (socket)
  (usocket:socket-close socket))

(defun send-request (socket request)
  (let ((peer (usocket:socket-stream socket)))
    (write-sequence request peer)
    (finish-output peer)
    (read-levin-packet peer)))

(defun test-request ()
  (let* ((socket (open-connection "127.0.0.1" 18080))
         (handshake-request (make-handshake-request))
         (flags-response (make-flags-response)))
    (print (deserialize-from-binary-storage (send-request socket handshake-request) 0))
    (print (deserialize-from-binary-storage (send-request socket flags-response) 0))
    (close-connection socket)))
