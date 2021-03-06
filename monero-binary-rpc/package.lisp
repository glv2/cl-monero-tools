;;;; This file is part of monero-tools
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :monero-binary-rpc
  (:use :cl :iterate :monero-rpc :monero-utils)
  (:import-from :monero-tools
                #:deserialize-from-binary-storage
                #:generate-secret-key
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
