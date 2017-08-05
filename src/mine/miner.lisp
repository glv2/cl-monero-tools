;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun miner (block-template-data reserve-size reserve-offset difficulty)
  "Given a block template represented by a BLOCK-TEMPLATE-DATA byte
vector containing a reserved space of RESERVE-SIZE bytes starting at
RESERVE-OFFSET in BLOCK-TEMPLATE-DATA in which an extra nonce can be
put, find a nonce and an extra nonce allowing the hash of the
resulting block (computed with slow-hash) to be acceptable for a given
DIFFICULTY level.
The first returned value is the new block data containing the found
nonces. The second returned value is the hash of the block (computed
with slow-hash)."
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
