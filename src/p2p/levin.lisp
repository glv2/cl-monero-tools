;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-p2p)


(defun make-request (command payload)
  (concatenate 'octet-vector
               (integer->bytes +levin-signature+ :size 8)
               (integer->bytes (length payload) :size 8)
               #(1)
               (integer->bytes command :size 4)
               (integer->bytes 0 :size 4)
               (integer->bytes +levin-packet-request+ :size 4)
               (integer->bytes +levin-protocol-version-1+ :size 4)
               payload))

(defun make-response (command payload return-code)
  (concatenate 'octet-vector
               (integer->bytes +levin-signature+ :size 8)
               (integer->bytes (length payload) :size 8)
               #(0)
               (integer->bytes command :size 4)
               (integer->bytes return-code :size 4)
               (integer->bytes +levin-packet-response+ :size 4)
               (integer->bytes +levin-protocol-version-1+ :size 4)
               payload))
