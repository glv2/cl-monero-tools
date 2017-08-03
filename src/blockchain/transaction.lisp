;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-transaction-hash (transaction)
  (let* ((prefix (geta transaction :prefix))
         (version (geta prefix :version)))
    (if (= 1 version)
        (fast-hash (serialize-transaction transaction))
        (let* ((rct-sig (geta transaction :rct-signature))
               (base (remove-if (lambda (x) (eq (car x) :rct-signature-prunable)) rct-sig))
               (prunable (geta rct-sig :rct-signature-prunable))
               (prefix-hash (fast-hash (serialize-transaction-prefix prefix)))
               (base-hash (fast-hash (serialize-rct-signature base)))
               (prunable-hash (if prunable
                                  (fast-hash (serialize-rct-signature-prunable prunable))
                                  (make-array +hash-length+
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0))))
          (fast-hash (concatenate '(simple-array (unsigned-byte 8) (*))
                                  prefix-hash
                                  base-hash
                                  prunable-hash))))))

(defun compute-transaction-hash-from-data (transaction-data)
  (compute-transaction-hash (deserialize-transaction transaction-data 0)))

(defun compute-miner-transaction-hash (block)
  (compute-transaction-hash (geta block :miner-transaction)))

(defun compute-miner-transaction-hash-from-data (block-data)
  (let* ((header-size (nth-value 1 (deserialize-block-header block-data 0)))
         (miner-transaction-size (nth-value 1 (deserialize-transaction block-data header-size))))
    (compute-transaction-hash-from-data (subseq block-data
                                                header-size
                                                (+ header-size miner-transaction-size)))))

(defun compute-transaction-tree-hash (hashes)
  (let ((count (length hashes))
        (data (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) (coerce hashes 'list))))
    (tree-hash data count)))
