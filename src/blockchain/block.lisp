;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-block-hash (block &optional slow-hash)
  "Return the hash of a BLOCK. The BLOCK must be in alist format. If
SLOW-HASH is not NIL, compute the hash using slow-hash (used for the
mining process) instead of fast-hash (used for the block id)."
  (let* ((block-data (serialize-block block))
         (block-data-hash (fast-hash block-data)))
    ;; Exception for block 202612 because there was a bug in the tree hash function
    (when (equalp block-data-hash
                  #.(hex-string->bytes "3a8a2b3a29b50fc86ff73dd087ea43c6f0d6b8f936c849194d5c84c737903966"))
      (return-from compute-block-hash
        (if slow-hash
            #.(hex-string->bytes "84f64766475d51837ac9efbef1926486e58563c95a19fef4aec3254f03000000")
            #.(hex-string->bytes "bbd604d2ba11ba27935e006ed39c9bfdd99b76bf4a50654bc1e1e61217962698"))))
    (let* ((header (geta block :header))
           (miner-transaction (geta block :miner-transaction))
           (miner-transaction-hash (compute-transaction-hash miner-transaction))
           (transaction-hashes (concatenate 'list
                                            (list miner-transaction-hash)
                                            (geta block :transaction-hashes)))
           (root-hash (compute-transaction-tree-hash transaction-hashes))
           (header-data (serialize-block-header header))
           (count (serialize-integer (length transaction-hashes)))
           (data (concatenate 'octet-vector header-data root-hash count))
           (size (serialize-integer (+ (length header-data)
                                       (length root-hash)
                                       (length count)))))
      (if slow-hash
          (slow-hash data)
          (fast-hash (concatenate 'octet-vector size data))))))

(defun compute-block-hash-from-data (block-data &optional slow-hash)
  "Return the hash of the block represented by the BLOCK-DATA byte
vector. If SLOW-HASH is not NIL, compute the hash using slow-hash
(used for the mining process) instead of fast-hash (used for the block
id)."
  (compute-block-hash (deserialize-block block-data 0) slow-hash))

(defun get-nonce-offset (block-data)
  "Return the offset in BLOCK-DATA indicating the beginning of the
4 bytes long nonce of the block represented by the BLOCK-DATA byte
vector."
  (nth-value 1 (deserialize block-data 0
                 ((major-version #'deserialize-integer)
                  (minor-version #'deserialize-integer)
                  (timestamp #'deserialize-integer)
                  (previous-block-hash #'deserialize-hash)))))

(defun acceptable-hash-p (hash difficulty)
  "Check if a block HASH (computed with slow-hash) is acceptable for
a given DIFFICULTY level."
  (< (* (bytes->integer hash) difficulty) #.(expt 2 256)))
