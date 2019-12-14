;;;; This file is part of monero-tools
;;;; Copyright 2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-utils"
  :name "monero-utils"
  :description "Utility functions for monero-tools"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("alexandria"
               "babel"
               "cffi"
               "cl-json"
               "ironclad")
  :components ((:module "monero-utils"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "base58")))))
