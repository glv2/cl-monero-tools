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
   ;; mine
   #:mine-block

   ;; rpc
   #:*rpc-host* #:*rpc-port* #:*rpc-user* #:*rpc-password*
   #:rpc #:json-rpc
   #:zmq-json-rpc

   ;; wallet
   #:transaction-history))
