;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools/tests)


(def-suite openalias-tests
  :description "Unit tests for openalias functions."
  :in monero-tools-tests)

(in-suite openalias-tests)

(test get-openalias-info
  (multiple-value-bind (info dnssec-validated)
      (get-openalias-info "donate.getmonero.org")
    (is (string= "44AFFq5kSiGBoZ4NMDwYtN18obc8AemS33DBLWs3H7otXft3XjrpDtQGv7SqSsaBYBb98uNbr2VBBEt7f2wfn3RVGQBEP3A"
                 (geta info :address)))
    (is (string= "Monero Development"
                 (geta info :recipient-name)))
    (is (string= "Donation to Monero Core Team"
                 (geta info :description)))
    (is-true dnssec-validated)))
