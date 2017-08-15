;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-transaction-hash (transaction)
  "Return the hash of a TRANSACTION. The TRANSACTION must be in
alist format."
  (let* ((prefix (geta transaction :prefix))
         (version (geta prefix :version)))
    (case version
      ((1) (fast-hash (serialize-transaction transaction)))
      ((2) (let* ((rct-sig (geta transaction :rct-signature))
                  (base (remove :rct-signature-prunable rct-sig :key #'car))
                  (prunable (geta rct-sig :rct-signature-prunable))
                  (prefix-hash (fast-hash (serialize-transaction-prefix prefix)))
                  (base-hash (fast-hash (serialize-rct-signature base)))
                  (prunable-hash (if prunable
                                     (fast-hash (serialize-rct-signature-prunable prunable))
                                     (make-array +hash-length+
                                                 :element-type '(unsigned-byte 8)
                                                 :initial-element 0))))
             (fast-hash (concatenate 'octet-vector prefix-hash base-hash prunable-hash))))
      (t (error "Transaction version ~d not supported." version)))))

(defun compute-transaction-hash-from-data (transaction-data)
  "Return the hash of the transaction represented by the
TRANSACTION-DATA byte vector."
  (compute-transaction-hash (deserialize-transaction transaction-data 0)))

(defun compute-miner-transaction-hash (block)
  "Return the hash of a BLOCK's reward transaction. The BLOCK must be in
alist format."
  (compute-transaction-hash (geta block :miner-transaction)))

(defun compute-miner-transaction-hash-from-data (block-data)
  "Return the hash of the reward transaction of the block represented
by the BLOCK-DATA byte vector."
  (let* ((header-size (nth-value 1 (deserialize-block-header block-data 0)))
         (miner-transaction-size (nth-value 1 (deserialize-transaction block-data header-size))))
    (compute-transaction-hash-from-data (subseq block-data
                                                header-size
                                                (+ header-size miner-transaction-size)))))

(defun compute-transaction-tree-hash (hashes)
  "Return the root of the Merkle tree computed from a list of
transaction HASHES."
  (let ((count (length hashes))
        (data (apply #'concatenate 'octet-vector (coerce hashes 'list))))
    (tree-hash data count)))
