;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun get-unix-time ()
  (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun make-handshake-request-payload (&key (network-id +p2p-network-id-mainnet+) (peer-id (random-bits 64)) (my-port 0))
  (let* ((network-id (coerce network-id 'octet-vector))
         (node-data (list (cons :local-time (cons (get-unix-time) '(unsigned-byte 64)))
                          (cons :my-port (cons my-port '(unsigned-byte 32)))
                          (cons :network-id (bytes->string network-id))
                          (cons :peer-id (cons peer-id '(unsigned-byte 64)))))
         (payload-data (list (cons :cumulative-difficulty (cons 1 '(unsigned-byte 64)))
                             (cons :current-height (cons 1 '(unsigned-byte 64)))
                             (cons :top-id (bytes->string +mainnet-genesis-hash+))
                             (cons :top-version (cons 1 '(unsigned-byte 8)))))
         (payload (list (cons :node-data node-data)
                        (cons :payload-data payload-data))))
    (serialize-to-binary-storage payload)))

(defun make-flags-request-payload ()
  (let ((payload (list (cons :support-flags (cons +p2p-support-flags+ '(unsigned-byte 8))))))
    (serialize-to-binary-storage payload)))

(defun close-connection (socket)
  (usocket:socket-close socket))

(defun open-connection (host port)
  (declare (optimize (debug 3)))
  (let* ((socket (usocket:socket-connect host port
                                         :protocol :stream
                                         :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket)))
    (clear-input stream)
    (write-request stream +p2p-command-handshake+ (make-handshake-request-payload))
    (multiple-value-bind (return-data-p command return-code flags payload)
        (read-levin-packet stream)
      (when (and return-data-p
                 (= command +p2p-command-request-support-flags+)
                 (= return-code +levin-ok+)
                 (= flags +levin-packet-request+))
        (write-response stream +p2p-command-request-support-flags+ (make-flags-request-payload) 0))
      (multiple-value-setq (return-data-p command return-code flags payload)
        (read-levin-packet stream))
      (unless (and (not return-data-p)
                   (= command +p2p-command-handshake+)
                   (= return-code 1)
                   (= flags +levin-packet-response+)
                   (plusp (length payload)))
        (close-connection socket)
        (error "Connection failed"))
      (let ((payload (deserialize-from-binary-storage payload 0)))
        (setf (geta payload :local-peerlist)
              (string->bytes (geta payload :local-peerlist)))
        (setf (geta (geta payload :node-data) :network-id)
              (string->bytes (geta (geta payload :node-data) :network-id)))
        (setf (geta (geta payload :payload-data) :top-id)
              (string->bytes (geta (geta payload :payload-data) :top-id)))
        (values socket payload)))))

(defun test-request ()
  (multiple-value-bind (socket payload)
      (open-connection "127.0.0.1" 18080)
    (close-connection socket)
    payload))
