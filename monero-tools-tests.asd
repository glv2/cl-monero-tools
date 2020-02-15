;;;; This file is part of monero-tools
;;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(cl:in-package :asdf-user)

(defsystem "monero-tools-tests"
  :name "monero-tools-tests"
  :description "Tests for monero-tools"
  :version "0.1"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :depends-on ("fiveam"
               "iterate"
               "monero-tools"
               "monero-utils"
               "uiop")
  :in-order-to ((test-op (load-op "monero-tools-tests")))
  :perform (test-op (op s)
                    (let ((tests (uiop:find-symbol* 'monero-tools-tests
                                                    :monero-tools-tests)))
                      (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :serial t
                :components ((:file "tests")
                             (:file "blockchain")
                             (:file "crypto")
                             (:file "mine")
                             (:file "mnemonic")
                             (:file "openalias")
                             (:file "serialization")
                             (:file "utils")
                             (:file "wallet")))))
