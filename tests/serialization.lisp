;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools/tests)


(def-suite serialization-tests
  :description "Unit tests for (de)serialization functions."
  :in monero-tools-tests)

(in-suite serialization-tests)

(test transaction
  (let ((non-rct-1 (load-hex-data "txn-c52bfd4006951d18be6f68bc3fd5da8ba982b8f26fa459545a7e13c9299e5cb3.hex"))
        (non-rct-2 (load-hex-data "txn-9df4b07f8d8f5f0a90b44d87576491d7ec69a282ed36fe36c27e9c67c46974df.hex"))
        (non-rct-several-ring-sizes (load-hex-data "txn-7ccdb8992b73d5fa9b284fef2387a4c8d599dc743685041093194be8187d3787.hex"))
        (rct-null (load-hex-data "txn-b2e4dc3ce1951e96310c8bc7dc2a76cac471b3ad6858ef8c6b864062c257518b.hex"))
        (rct-full (load-hex-data "txn-140564273396a16135ba0867ded6b7981fdc28bda45c62f993dc51ff26cfb2e5.hex"))
        (rct-simple (load-hex-data "txn-467f1914b3f5f4eb52dda02bfd0b70b89722b88063f40889bfba46d3ec78de80.hex"))
        (rct-payment-id (load-hex-data "txn-0a989c65415dac7840cd890dd5c514b7bfaadf4041a9144e734aef30e3ffa5ec.hex"))
        (rct-encrypted-payment-id (load-hex-data "txn-dcf6d169682f243ab25a627d5540882c39fe376526184fa356ee086a68ae164e.hex"))
        (rct-bulletproof-1 (load-hex-data "txn-dced075926833171dd9bd9684949aef89451a54dece9e29a84bd843bf86f09dd.hex"))
        (rct-bulletproof-2 (load-hex-data "txn-cf43b200852e0691161e01448e1bcef9595d113b25a9e21342854a4cd7682676.hex")))
    (flet ((reserialize/hex (transaction)
             (let ((deserialized (deserialize-transaction (hex-string->bytes transaction) 0)))
               (bytes->hex-string (serialize-transaction nil deserialized)))))
      (is (string-equal non-rct-1 (reserialize/hex non-rct-1)))
      (is (string-equal non-rct-2 (reserialize/hex non-rct-2)))
      (is (string-equal non-rct-several-ring-sizes (reserialize/hex non-rct-several-ring-sizes)))
      (is (string-equal rct-null (reserialize/hex rct-null)))
      (is (string-equal rct-full (reserialize/hex rct-full)))
      (is (string-equal rct-simple (reserialize/hex rct-simple)))
      (is (string-equal rct-payment-id (reserialize/hex rct-payment-id)))
      (is (string-equal rct-encrypted-payment-id (reserialize/hex rct-encrypted-payment-id)))
      (is (string-equal rct-bulletproof-1 (reserialize/hex rct-bulletproof-1)))
      (is (string-equal rct-bulletproof-2 (reserialize/hex rct-bulletproof-2))))))

(test block
  (let ((block-400000 (load-hex-data "blk-400000.hex"))
        (block-1000000 (load-hex-data "blk-1000000.hex"))
        (block-1300000 (load-hex-data "blk-1300000.hex")))
    (flet ((reserialize/hex (block)
             (let ((deserialized (deserialize-block (hex-string->bytes block) 0)))
               (bytes->hex-string (serialize-block nil deserialized)))))
      (is (string-equal block-400000 (reserialize/hex block-400000)))
      (is (string-equal block-1000000 (reserialize/hex block-1000000)))
      (is (string-equal block-1300000 (reserialize/hex block-1300000))))))

(test binary-storage
  (let ((block-400000 (load-hex-data "blk-400000.hex"))
        (block-1000000 (load-hex-data "blk-1000000.hex"))
        (block-1300000 (load-hex-data "blk-1300000.hex"))
        (transaction-hashes-1300000 #("140564273396a16135ba0867ded6b7981fdc28bda45c62f993dc51ff26cfb2e5"
                                      "a32087d20f25e45097da9c899d8ec17df1d7563abe19047b3d115fe894bbf383"
                                      "4d2996d78485bd41980c79a7573e91fb06960a96884eda6b47877be8bc0e4eb4"))
        (get-blocks-by-height-bin-answer (load-hex-data "get-blocks-by-height-bin-400000+1000000+1300000.hex")))
    (let* ((info (deserialize-from-binary-storage (hex-string->bytes get-blocks-by-height-bin-answer) 0))
           (blk-400000 (string->bytes (geta (aref (geta info :blocks) 0) :block)))
           (blk-1000000 (string->bytes (geta (aref (geta info :blocks) 1) :block)))
           (blk-1300000 (string->bytes (geta (aref (geta info :blocks) 2) :block)))
           (txs-1300000 (geta (aref (geta info :blocks) 2) :txs))
           (txhs-1300000 (map 'vector
                              (lambda (tx)
                                (compute-transaction-hash-from-data (string->bytes tx)))
                              txs-1300000)))
      (is (string-equal block-400000 (bytes->hex-string blk-400000)))
      (is (string-equal block-1000000 (bytes->hex-string blk-1000000)))
      (is (string-equal block-1300000 (bytes->hex-string blk-1300000)))
      (dotimes (i (length transaction-hashes-1300000))
        (is (string-equal (aref transaction-hashes-1300000 i)
                          (bytes->hex-string (aref txhs-1300000 i)))))
      (is (string-equal get-blocks-by-height-bin-answer
                        (bytes->hex-string (serialize-to-binary-storage info)))))))
