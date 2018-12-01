;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-tools-p2p"
  :name "monero-tools-p2p"
  :description "P2P tools to work with the Monero crypto-currency"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("dexador"
               "monero-tools")
  :components ((:module "src"
                :components ((:file "package-p2p")
                             (:module "p2p"
                              :depends-on ("package-p2p")
                              :components ((:file "constants")
                                           (:file "levin" :depends-on ("constants"))
                                           (:file "p2p" :depends-on ("constants" "levin"))))))))
