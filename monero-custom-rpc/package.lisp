;;;; This file is part of monero-tools
;;;; Copyright 2019-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-custom-rpc
  (:use :cl
        :iterate
        :monero-binary-rpc
        :monero-daemon-rpc
        :monero-rpc
        :monero-tools
        :monero-utils)
  (:import-from :bordeaux-threads
                #:join-thread
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:export
   #:mine-block
   #:transaction-history))
