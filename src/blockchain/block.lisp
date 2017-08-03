;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(defun compute-block-hash (block &optional slow-hash)
  (let* ((block-data (serialize-block block))
         (block-data-hash (fast-hash block-data)))
    ;; Exception for block 202612
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
           (data (concatenate '(simple-array (unsigned-byte 8) (*))
                              header-data root-hash count))
           (size (serialize-integer (+ (length header-data)
                                       (length root-hash)
                                       (length count)))))
      (if slow-hash
          (slow-hash data)
          (fast-hash (concatenate '(simple-array (unsigned-byte 8) (*)) size data))))))

(defun compute-block-hash-from-data (block-data &optional slow-hash)
  (compute-block-hash (deserialize-block block-data 0) slow-hash))

(defun get-nonce-offset (block-data)
  (nth-value 1 (deserialize block-data 0
                            ((major-version #'deserialize-integer)
                             (minor-version #'deserialize-integer)
                             (timestamp #'deserialize-integer)
                             (previous-block-hash #'deserialize-hash)))))

(defun check-block-hash (hash difficulty)
  (< (* (bytes->integer hash) difficulty) #.(expt 2 256)))
