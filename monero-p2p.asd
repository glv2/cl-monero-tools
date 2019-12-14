;;;; This file is part of monero-tools
;;;; Copyright 2018-2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-p2p"
  :name "monero-p2p"
  :description "P2P tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("alexandria"
               "dexador"
               "ironclad"
               "monero-tools"
               "monero-utils")
  :components ((:module "monero-p2p"
                :serial t
                :components ((:file "package")
                             (:file "constants")
                             (:file "levin")
                             (:file "p2p")))))
