;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-tools-p2p
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:import-from :ironclad
                #:random-data)
  (:import-from :monero-tools
                #:+mainnet-genesis-hash+
                #:integer->bytes
                #:octet-vector
                #:serialize-to-binary-storage))
