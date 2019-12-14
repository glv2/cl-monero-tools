;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-rpc"
  :name "monero-rpc"
  :description "RPC tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("cl-base64"
               "cl-json"
               "dexador"
               "ironclad"
               "monero-utils"
               "split-sequence")
  :components ((:module "monero-rpc"
                :serial t
                :components ((:file "package")
                             (:file "rpc")
                             (:file "daemon")
                             (:file "wallet")))))
