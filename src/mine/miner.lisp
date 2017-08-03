;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun miner (block-template-data reserve-size reserve-offset difficulty)
  (let ((*random-state* (make-random-state t)))
    (loop with nonce-offset = (get-nonce-offset block-template-data)
          with template = (copy-seq block-template-data)
          for hash = (compute-block-hash-from-data template t)
          until (acceptable-hash-p hash difficulty)
          do (progn
               (loop for i from nonce-offset below (+ nonce-offset 4)
                     do (setf (aref template i) (random 256)))
               (loop for i from reserve-offset below (+ reserve-offset reserve-size)
                     do (setf (aref template i) (random 256))))
          finally (return (values template hash)))))
