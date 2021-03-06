;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite blockchain-tests
  :description "Unit tests for blockchain functions."
  :in monero-tools-tests)

(in-suite blockchain-tests)

(test compute-transaction-hash-from-data
  (let ((non-rct-1 (load-hex-data "txn-c52bfd4006951d18be6f68bc3fd5da8ba982b8f26fa459545a7e13c9299e5cb3.hex"))
        (non-rct-2 (load-hex-data "txn-9df4b07f8d8f5f0a90b44d87576491d7ec69a282ed36fe36c27e9c67c46974df.hex"))
        (rct-null (load-hex-data "txn-b2e4dc3ce1951e96310c8bc7dc2a76cac471b3ad6858ef8c6b864062c257518b.hex"))
        (rct-full (load-hex-data "txn-140564273396a16135ba0867ded6b7981fdc28bda45c62f993dc51ff26cfb2e5.hex"))
        (rct-simple (load-hex-data "txn-467f1914b3f5f4eb52dda02bfd0b70b89722b88063f40889bfba46d3ec78de80.hex"))
        (rct-bulletproof-1 (load-hex-data "txn-dced075926833171dd9bd9684949aef89451a54dece9e29a84bd843bf86f09dd.hex"))
        (rct-bulletproof-2 (load-hex-data "txn-cf43b200852e0691161e01448e1bcef9595d113b25a9e21342854a4cd7682676.hex"))
        (rct-bulletproof-3 (load-hex-data "txn-3c24dc3ec70015789d965a8e516383bf7866e9a46e7665c962ff990e216f51b2.hex")))
    (flet ((compute-hash/hex (transaction)
             (let ((transaction-data (hex-string->bytes transaction)))
               (bytes->hex-string (compute-transaction-hash-from-data transaction-data)))))
      (is (string-equal "c52bfd4006951d18be6f68bc3fd5da8ba982b8f26fa459545a7e13c9299e5cb3"
                        (compute-hash/hex non-rct-1)))
      (is (string-equal "9df4b07f8d8f5f0a90b44d87576491d7ec69a282ed36fe36c27e9c67c46974df"
                        (compute-hash/hex non-rct-2)))
      (is (string-equal "b2e4dc3ce1951e96310c8bc7dc2a76cac471b3ad6858ef8c6b864062c257518b"
                        (compute-hash/hex rct-null)))
      (is (string-equal "140564273396a16135ba0867ded6b7981fdc28bda45c62f993dc51ff26cfb2e5"
                        (compute-hash/hex rct-full)))
      (is (string-equal "467f1914b3f5f4eb52dda02bfd0b70b89722b88063f40889bfba46d3ec78de80"
                        (compute-hash/hex rct-simple)))
      (is (string-equal "dced075926833171dd9bd9684949aef89451a54dece9e29a84bd843bf86f09dd"
                        (compute-hash/hex rct-bulletproof-1)))
      (is (string-equal "cf43b200852e0691161e01448e1bcef9595d113b25a9e21342854a4cd7682676"
                        (compute-hash/hex rct-bulletproof-2)))
      (is (string-equal "3c24dc3ec70015789d965a8e516383bf7866e9a46e7665c962ff990e216f51b2"
                        (compute-hash/hex rct-bulletproof-3))))))

(test compute-miner-transaction-hash-from-data
  (let ((block-400000 (load-hex-data "blk-400000.hex"))
        (block-1000000 (load-hex-data "blk-1000000.hex"))
        (block-1300000 (load-hex-data "blk-1300000.hex")))
    (flet ((compute-hash/hex (block)
             (let ((block-data (hex-string->bytes block)))
               (bytes->hex-string (compute-miner-transaction-hash-from-data block-data)))))
      (is (string-equal "c52bfd4006951d18be6f68bc3fd5da8ba982b8f26fa459545a7e13c9299e5cb3"
                        (compute-hash/hex block-400000)))
      (is (string-equal "eb4670d141a7474b07426c34f5cd0cb54dfd5bade712abdec5dbd9f8cf0fc958"
                        (compute-hash/hex block-1000000)))
      (is (string-equal "b2e4dc3ce1951e96310c8bc7dc2a76cac471b3ad6858ef8c6b864062c257518b"
                        (compute-hash/hex block-1300000))))))

(test compute-block-hash-from-data
  (let ((block-400000 (load-hex-data "blk-400000.hex"))
        (block-1000000 (load-hex-data "blk-1000000.hex"))
        (block-1300000 (load-hex-data "blk-1300000.hex")))
    (flet ((compute-hash/hex (block)
             (let ((block-data (hex-string->bytes block)))
               (bytes->hex-string (compute-block-hash-from-data block-data)))))
      (is (string-equal "1b2b0e7a30e59691491529a3d506d1ba3d6052d0f6b52198b7330b28a6f1b6ac"
                        (compute-hash/hex block-400000)))
      (is (string-equal "a886ef5149902d8342475fee9bb296341b891ac67c4842f47a833f23c00ed721"
                        (compute-hash/hex block-1000000)))
      (is (string-equal "31b34272343a44a9f4ac7de7a8fcf3b7d8a3124d7d6870affd510d2f37e74cd0"
                        (compute-hash/hex block-1300000))))))

(test acceptable-hash-p
  (let ((block-400000 (load-hex-data "blk-400000.hex"))
        (difficulty-400000 778257991)
        (block-1000000 (load-hex-data "blk-1000000.hex"))
        (difficulty-1000000 897275644)
        (block-1300000 (load-hex-data "blk-1300000.hex"))
        (difficulty-1300000 7877790006))
    (flet ((hash/hex (block)
             (compute-block-hash-from-data (hex-string->bytes block) t)))
      (is-true (acceptable-hash-p (hash/hex block-400000) difficulty-400000))
      (is-true (acceptable-hash-p (hash/hex block-1000000) difficulty-1000000))
      (is-true (acceptable-hash-p (hash/hex block-1300000) difficulty-1300000))
      (is-false (acceptable-hash-p (hash/hex block-400000) difficulty-1300000)))))
