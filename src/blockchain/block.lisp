;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-block-hash (block)
  (let* ((block-data (serialize-block block))
         (block-data-hash (bytes->hex-string (fast-hash block-data))))
    ;; Exception for block 202612
    (when (string-equal block-data-hash "3a8a2b3a29b50fc86ff73dd087ea43c6f0d6b8f936c849194d5c84c737903966")
      (return-from compute-block-hash "bbd604d2ba11ba27935e006ed39c9bfdd99b76bf4a50654bc1e1e61217962698"))
    (let* ((header (geta block :header))
           (miner-transaction (geta block :miner-transaction))
           (miner-transaction-hash (compute-transaction-hash miner-transaction))
           (transaction-hashes (concatenate 'list
                                            (list miner-transaction-hash)
                                            (geta block :transaction-hashes)))
           (root-hash (compute-transaction-tree-hash transaction-hashes))
           (header-data (write-block-header header))
           (root-hash-data (hex-string->bytes root-hash))
           (count-data (write-varint (length transaction-hashes)))
           (size-data (write-varint (+ (length header-data)
                                       (length root-hash-data)
                                       (length count-data))))
           (data (concatenate '(simple-array (unsigned-byte 8) (*))
                              size-data header-data root-hash-data count-data)))
      (bytes->hex-string (fast-hash data)))))

(defun compute-block-hash-from-data (block-data)
  (let ((block-data-hash (bytes->hex-string (fast-hash block-data))))
    ;; Exception for block 202612
    (when (string-equal block-data-hash "3a8a2b3a29b50fc86ff73dd087ea43c6f0d6b8f936c849194d5c84c737903966")
      (return-from compute-block-hash-from-data "bbd604d2ba11ba27935e006ed39c9bfdd99b76bf4a50654bc1e1e61217962698"))
    (compute-block-hash (deserialize-block block-data))))
