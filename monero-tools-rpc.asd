;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-tools-rpc"
  :name "monero-tools-rpc"
  :description "RPC tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("bordeaux-threads"
               "cl-base64"
               "cl-json"
               "dexador"
               "ironclad"
               "monero-tools"
               "pzmq"
               "split-sequence")
  :components ((:module "src"
                :components ((:file "package-rpc")
                             (:module "rpc"
                              :depends-on ("package-rpc")
                              :components ((:file "daemon" :depends-on ("rpc" "zmq"))
                                           (:file "history" :depends-on ("daemon"))
                                           (:file "miner" :depends-on ("daemon"))
                                           (:file "rpc")
                                           (:file "wallet" :depends-on ("rpc" "zmq"))
                                           (:file "zmq" :depends-on ("rpc"))))))))
