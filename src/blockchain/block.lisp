;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools)


(define-constant +mainnet-genesis-hash+
    #.(hex-string->bytes "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3")
  :test #'equalp)
(define-constant +testnet-genesis-hash+
    #.(hex-string->bytes "48ca7cd3c8de5b6a4d53d2861fbdaedca141553559f9be9520068053cda8430b")
  :test #'equalp)
(define-constant +stagenet-genesis-hash+
    #.(hex-string->bytes "76ee3cc98646292206cd3e86f74d88b4dcc1d937088645e9b0cbca84b7ce74eb")
  :test #'equalp)

(defun transaction-hashes (block)
  "Return the hashes of the transactions in a BLOCK (miner transaction
included)."
  (concatenate 'list
               (list (compute-transaction-hash (geta block :miner-transaction)))
               (geta block :transaction-hashes)))

(defun compute-block-hash (block &optional slow-hash)
  "Return the hash of a BLOCK. The BLOCK must be in alist format. If
SLOW-HASH is not NIL, compute the hash using slow-hash (used for the
mining process) instead of fast-hash (used for the block id)."
  (let* ((block-data (serialize-block nil block))
         (block-data-hash (fast-hash block-data)))
    ;; Exception for block 202612 because there was a bug in the tree hash function
    (when (equalp block-data-hash
                  #.(hex-string->bytes "3a8a2b3a29b50fc86ff73dd087ea43c6f0d6b8f936c849194d5c84c737903966"))
      (return-from compute-block-hash
        (if slow-hash
            #.(hex-string->bytes "84f64766475d51837ac9efbef1926486e58563c95a19fef4aec3254f03000000")
            #.(hex-string->bytes "bbd604d2ba11ba27935e006ed39c9bfdd99b76bf4a50654bc1e1e61217962698"))))
    (let* ((header (geta block :header))
           (transaction-hashes (transaction-hashes block))
           (root-hash (compute-transaction-tree-hash transaction-hashes))
           (data (with-octet-output-stream (result)
                   (serialize-block-header result header)
                   (write-sequence root-hash result)
                   (serialize-integer result (length transaction-hashes)))))
      (if slow-hash
          (slow-hash data)
          (fast-hash (concatenate 'octet-vector
                                  (serialize-integer nil (length data))
                                  data))))))

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
