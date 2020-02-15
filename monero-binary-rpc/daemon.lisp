;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-binary-rpc)


(defbinrpc get-blocks.bin ("get_blocks.bin" block-ids &key start-height prune no-miner-transaction)
  "Get all blocks info."
  (let ((hex-block-ids (with-output-to-string (s)
                         (dolist (id (coerce block-ids 'list))
                           (write-string id s)))))
    (append (list (cons "block_ids" (bytes->string (hex-string->bytes hex-block-ids))))
            (when start-height
              (list (cons "start_height" start-height)))
            (when prune
              (list (cons "prune" t)))
            (when no-miner-transaction
              (list (cons "no_miner_tx" t)))))
  (lambda (result)
    (let ((blocks (geta result :blocks)))
      (dotimes (i (length blocks))
        (let ((b (aref blocks i)))
          (setf (geta b :block) (string->bytes (geta b :block))))))
    result))

;; (get-blocks.bin (list "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148" "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3"))

(defbinrpc get-blocks-by-height.bin ("get_blocks_by_height.bin" heights)
  "Get blocks by height."
  (list (cons "heights" (coerce heights '(simple-array (unsigned-byte 64) (*)))))
  (lambda (result)
    (let ((blocks (geta result :blocks)))
      (dotimes (i (length blocks))
        (let ((b (aref blocks i)))
          (setf (geta b :block) (string->bytes (geta b :block))))))
    result))

(defbinrpc get-hashes.bin ("get_hashes.bin" block-ids &key start-height)
  "Get hashes."
  (let ((hex-block-ids (with-output-to-string (s)
                         (dolist (id (coerce block-ids 'list))
                           (write-string id s)))))
    (append (list (cons "block_ids" (bytes->string (hex-string->bytes hex-block-ids))))
            (when start-height
              (list (cons "start_height" start-height)))))
  (lambda (result)
    (let* ((data-string (geta result :m-block-ids))
           (size (length data-string))
           (key-length 32))
      (unless (zerop (mod size key-length))
        (error "Invalid length"))
      (setf (geta result :m-block-ids)
            (map 'vector
                 #'string->bytes
                 (iter (for i from 0 below size by key-length)
                       (collect (subseq data-string i (+ i key-length)))))))
    result))

;; (get-hashes.bin (list "771fbcd656ec1464d3a02ead5e18644030007a0fc664c0a964d30922821a8148" "418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e3"))

(defbinrpc get-o-indexes.bin ("get_o_indexes.bin" transaction-id)
  "Get global output indexes of a transaction."
  (list (cons "txid" (bytes->string (hex-string->bytes transaction-id)))))

(defbinrpc get-outs.bin ("get_outs.bin" outputs &key (get-transaction-id t))
  "Get outputs."
  (list (cons "outputs" (coerce outputs 'vector))
        (cons "get_txid" (when get-transaction-id t)))
  (lambda (result)
    (let ((outs (geta result :outs)))
      (dotimes (i (length outs))
        (let ((out (aref outs i)))
          (setf (geta out :key) (string->bytes (geta out :key)))
          (setf (geta out :mask) (string->bytes (geta out :mask)))
          (setf (geta out :txid) (string->bytes (geta out :txid))))))
    result))

(defbinrpc get-random-outs.bin ("get_random_outs.bin" amounts output-count)
  "Get a list of random outputs for a specific list of amounts."
  (list (cons "amounts" (coerce amounts 'vector))
        (cons "outs_count" output-count))
  (lambda (result)
    (let ((outs (geta result :outs)))
      (dotimes (i (length outs))
        (let* ((x (aref outs i))
               (y (string->bytes (geta x :outs)))
               (z (iter (for j from 0 below (length y) by 40)
                        (collect (list (cons :amount-index
                                             (bytes->integer y
                                                             :start j
                                                             :end (+ j 8)))
                                       (cons :output-key
                                             (subseq y (+ j 8) (+ j 40))))))))
          (setf (geta x :outs) z))))
    result))

(defbinrpc get-random-rctouts.bin ("get_random_rctouts.bin" output-count)
  "Get random RingCT outputs."
  (list (cons "outs_count" output-count))
  (lambda (result)
    (let* ((outs (geta result :outs))
           (x (string->bytes outs))
           (y (iter (for i from 0 below (length x) by 80)
                    (collect (list (cons :amount
                                         (bytes->integer x
                                                         :start i
                                                         :end (+ i 8)))
                                   (cons :amount-index
                                         (bytes->integer x
                                                         :start (+ i 8)
                                                         :end (+ i 16)))
                                   (cons :output-key
                                         (subseq x (+ i 16) (+ i 48)))
                                   (cons :commitment
                                         (subseq x (+ i 48) (+ i 80))))))))
      (setf (geta result :outs) y))
    result))

(defrawrpc get-transaction-pool-hashes.bin ("get_transaction_pool_hashes.bin")
  "Get hashes from transaction pool."
  nil
  (lambda (result)
    (let* ((result (let ((json:*use-strict-json-rules* nil))
                     (decode-json-from-string (bytes->string result))))
           (data (geta result :tx-hashes))
           (data-string (when data
                          (string->bytes data)))
           (transaction-hashes (when data-string
                                 (iter (for i from 0 below (length data-string) by 32)
                                       (collect (subseq data-string i (+ i 32)))))))
      (when transaction-hashes
        (setf (geta result :tx-hashes) transaction-hashes))
      result)))
