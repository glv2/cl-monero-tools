;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun encrypt-payment-id (payment-id public-view-key transaction-secret-key)
  "Encrypt a PAYMENT-ID using a shared secret derived from
a PUBLIC-VIEW-KEY and a TRANSACTION-SECRET-KEY."
  (let ((key (derive-key public-view-key transaction-secret-key)))
    (map '(simple-array (unsigned-byte 8) (*)) #'logxor payment-id key)))

(defun decrypt-payment-id (encrypted-payment-id transaction-public-key secret-view-key)
  "Decrypt an ENCRYPTED-PAYMENT-ID using a shared secret derived-from
a TRANSACTION-PUBLIC-KEY and a SECRET-VIEW-KEY."
  (let ((key (derive-key transaction-public-key secret-view-key)))
    (map '(simple-array (unsigned-byte 8) (*)) #'logxor encrypted-payment-id key)))
