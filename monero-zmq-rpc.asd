;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-zmq-rpc"
  :name "monero-zmq-rpc"
  :description "RPC tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("monero-rpc"
               "monero-utils"
               "pzmq")
  :components ((:module "monero-zmq-rpc"
                :serial t
                :components ((:file "package")
                             (:file "zmq")
                             (:file "daemon")))))
