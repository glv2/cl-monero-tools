;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-custom-rpc"
  :name "monero-custom-rpc"
  :description "RPC tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("bordeaux-threads"
               "monero-binary-rpc"
               "monero-rpc"
               "monero-tools"
               "monero-utils")
  :components ((:module "monero-custom-rpc"
                :serial t
                :components ((:file "package")
                             (:file "history")
                             (:file "miner")))))
