;;;; This file is part of monero-tools
;;;; Copyright 2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :monero-tools)


(defconstant +seed-hash-epoch-blocks+ 2048)
(defconstant +seed-hash-epoch-lag+ 64)

(defun randomx-seed-height (height)
  "Find the height at which RandomX was last seeded."
  (if (<= height (+ +seed-hash-epoch-blocks+ +seed-hash-epoch-lag+))
      0
      (logand (- height +seed-hash-epoch-lag+ 1)
              (lognot (1- +seed-hash-epoch-blocks+)))))

(defun randomx (data seed)
  (let* ((info (ignore-errors (randomx-service-info)))
         (current-seed (geta info :seed)))
    (unless info
      (error "Connection to randomx-service failed."))
    (unless (and current-seed
                 (string= current-seed (bytes->hex-string seed)))
      (randomx-service-reseed seed))
    (randomx-service-hash data :seed seed)))
