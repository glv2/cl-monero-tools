;;;; This file is part of monero-tools
;;;; Copyright 2018-2019 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-binary-rpc
  (:use :cl :monero-rpc :monero-utils)
  (:import-from :monero-tools
                #:deserialize-from-binary-storage
                #:serialize-to-binary-storage)
  (:export
   #:binary-rpc
   #:defbinrpc

   #:get-blocks.bin
   #:get-blocks-by-height.bin
   #:get-hashes.bin
   #:get-o-indexes.bin
   #:get-outs.bin
   #:get-random-outs.bin
   #:get-random-rctouts.bin
   #:get-transaction-pool-hashes.bin))
