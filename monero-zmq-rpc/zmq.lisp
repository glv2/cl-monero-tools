;;;; This file is part of monero-tools
;;;; Copyright 2016-2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-zmq-rpc)


(defun zmq-json-rpc (method &key parameters (rpc-host *rpc-host*) (rpc-port *rpc-port*) (rpc-user *rpc-user*) (rpc-password *rpc-password*))
  "Send a METHOD JSON RPC request to RPC-HOST:RPC-PORT with optional
PARAMETERS."
  (declare (ignore rpc-user rpc-password))
  (let* ((server-uri (format nil "tcp://~a:~a" rpc-host rpc-port))
         (request (list (cons :jsonrpc "2.0")
                        (cons :id (random 1000000))
                        (cons :method (string-downcase (string method)))
                        (cons :params parameters)))
         (json-request (encode-json-to-string request)))
    (pzmq:with-socket socket :req
      (pzmq:connect socket server-uri)
      (pzmq:send socket json-request)
      (let* ((json-answer (pzmq:recv-string socket))
             (answer (decode-json-from-string json-answer))
             (err (geta answer :error)))
        (if err
            (error (geta err :message))
            (geta answer :result))))))
