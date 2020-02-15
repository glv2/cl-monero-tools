;;;; This file is part of monero-tools
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-p2p
  (:use :cl :iterate :monero-utils)
  (:import-from :alexandria
                #:define-constant)
  (:import-from :ironclad
                #:random-bits)
  (:import-from :monero-tools
                #:+mainnet-genesis-hash+
                #:deserialize-from-binary-storage
                #:serialize-to-binary-storage)
  (:import-from :usocket
                #:socket-connect
                #:socket-close
                #:socket-stream)
  (:export
   #:network-id*
   #:*p2p-port*
   #:*peer-id*
   #:close-connection
   #:open-connection))
