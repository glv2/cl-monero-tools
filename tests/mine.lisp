;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite mine-tests
  :description "Unit tests for mining functions."
  :in monero-tools-tests)

(in-suite mine-tests)

#+sbcl
(test miner
  (let ((template-1 "0100d186c49a05418015bb9ae982a1975da7d79277c2705727a56894ba0fb246adaabb1f4632e300000000013d01ff0109c0a710025f391ad90e8b64ef432a0a28d0d9d699189b9c1322248ddbdb699c101a3cd9c3c0a8a5040233b81ac838467f7c59079e80517be1a45de51248a9bd841bf783e0aca7b2d5e2808ece1c023dc61a8cab11504ef1a36c51affeb7eed222cd09c98bf5045e489c23b409c9bc80c2d72f022caa4a9af327b36df136599a104278cf44342b0732bb5ad8c4c3582531add94b80a8d6b90702f9edf7d361176a29d1ce53e890e6ebc1e721d9719d273b69f1eb15d96025c6348088aca3cf02022e5ba472765cb2297f51f1a47aaf5f358cf48e16593c25bfc1880578014a65ed8090cad2c60e021b82a36cde1c9a6103d99ea6ebfb9f82b2547cef8934d8e87cf5e20c098ede9c80e08d84ddcb0102b37974cac8678446a230fb3e1b0ec06d56666bf0bf43dd64ff096a01f5aa2fda80c0caf384a30202a0c492751cb53389c9228f68b1ec1be831fb774a285f06c4ee02053858486e8221012a9fca96074c5216f9622c58c5c95024e53ff579d128913548363cb14d7f637400")
        (difficulty-1 1)
        (template-3 "0100d186c49a05e2914694b698fb417eebaebad3fed4d13e8f670119c95c6e3fa83c1d4531152000000000013f01ff0309efc02b029c0988ea47eafe15303fe7b37e8f4e15a51273681ecaf2b678745d5161d875a3c096b102021b1c293db480f77f6cfa676bc648cbc9dc41c8721fd2c9a16829d5efd75b884a8087a70e02e17ecb617676a95153fa9dd5fcfdb8bb94bb9eb494025c2575633318c9ad6dbb80c2d72f02109215edd47b2a7994315a3079a03c72ec5a8ab20df0cc5e7a44b6143d85cbc980a8d6b907029537e0e736baa6c8c9890e73a2d01e0194f627ae51619c138729441fa046d6f08088aca3cf0202de5f602b8c1da008da4692d017b1955e5cd41655d13e037e8e40cc61edecbca58090cad2c60e021ef910e155d7216724158f1a6cce3ce9f0e25216b7c0f064328dde2fbabd63e480e08d84ddcb010274450959e6dcce686e7fdb8e319cfca01360a93be70eb91ab6b870384366ba1e80c0caf384a30202bca1c2f25f636a8fa5ce5a9fc7c10e5a29aa9ee5d9c42ada34a1a3fc3ec95bc121010c39712ea3adf6e6fe08ebc970ef277e48b80867208ed2e60e862417a715ba7700")
        (difficulty-3 60))
    (flet ((miner/hex (template difficulty)
             (miner (hex-string->bytes template) 0 0 difficulty)))
      (is-true (acceptable-hash-p (compute-block-hash-from-data (miner/hex template-1 difficulty-1) t) difficulty-1))
      (is-true (acceptable-hash-p (compute-block-hash-from-data (miner/hex template-3 difficulty-3) t) difficulty-3)))))
