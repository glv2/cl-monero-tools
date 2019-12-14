;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun mining-profitability (difficulty block-reward price hash-rate days)
  "Compute the expected gain when mining with some HASH-RATE during a number of
DAYS given the network DIFFICULTY, the BLOCK-REWARD and the PRICE of a coin."
  (let* ((global-hash-rate (/ difficulty +block-time+))
         (ratio (/ hash-rate global-hash-rate))
         (gain-per-block (* block-reward price ratio))
         (blocks-per-day (/ (* 24 60 60) +block-time+))
         (num-blocks (* days blocks-per-day)))
    (* gain-per-block num-blocks)))
