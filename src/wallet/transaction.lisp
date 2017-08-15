;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defconstant +encrypted-payment-id-tail+ 141)

(defun encrypt-payment-id (payment-id public-view-key transaction-secret-key)
  "Encrypt a PAYMENT-ID using a shared secret derived from
a PUBLIC-VIEW-KEY and a TRANSACTION-SECRET-KEY."
  (let* ((derivation (derive-key public-view-key transaction-secret-key))
         (data (concatenate 'octet-vector derivation (vector +encrypted-payment-id-tail+)))
         (key (fast-hash data)))
    (map 'octet-vector #'logxor payment-id key)))

(defun decrypt-payment-id (encrypted-payment-id transaction-public-key secret-view-key)
  "Decrypt an ENCRYPTED-PAYMENT-ID using a shared secret derived-from
a TRANSACTION-PUBLIC-KEY and a SECRET-VIEW-KEY."
  (encrypt-payment-id encrypted-payment-id transaction-public-key secret-view-key))

(defun output-for-address-p (output-key output-index transaction-public-key address secret-view-key)
  "Check if an ADDRESS is the destination of an output."
  (let* ((public-spend-key (geta (decode-address address) :public-spend-key))
         (k (derive-key transaction-public-key secret-view-key))
         (pk (derive-public-key k output-index public-spend-key)))
    (equalp pk output-key)))
