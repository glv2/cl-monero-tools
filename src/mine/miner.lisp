;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defparameter *mine-stop* nil)
(defparameter *mine-result* nil)
(defparameter *mine-lock* nil)

(defun miner (block-template-data reserve-size reserve-offset difficulty)
  "Given a block template represented by a BLOCK-TEMPLATE-DATA byte
vector containing a reserved space of RESERVE-SIZE bytes starting at
RESERVE-OFFSET in BLOCK-TEMPLATE-DATA in which an extra nonce can be
put, find a nonce and an extra nonce allowing the hash of the
resulting block (computed with slow-hash) to be acceptable for a given
DIFFICULTY level.
The returned value is the new block data containing the found nonces."
  (loop with nonce-offset = (get-nonce-offset block-template-data)
        with template = (copy-seq block-template-data)
        for hash = (compute-block-hash-from-data template t)
        until (or *mine-stop* (acceptable-hash-p hash difficulty))
        do (let ((random-data (ironclad:random-data (+ 4 reserve-size))))
             (replace template random-data :start1 nonce-offset :end2 4)
             (replace template random-data :start1 reserve-offset :start2 4))
        finally (return (when (acceptable-hash-p hash difficulty)
                          (if *mine-lock*
                              (progn
                                (with-lock-held (*mine-lock*)
                                  (setf *mine-stop* t)
                                  (setf *mine-result* template))
                                *mine-result*)
                              template)))))
