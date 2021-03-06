;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite utils-tests
  :description "Unit tests for utility functions."
  :in monero-tools-tests)

(in-suite utils-tests)

(test base58-encode
  (flet ((base58-encode/hex (data)
           (base58-encode (hex-string->bytes data))))
    (is (string= (base58-encode/hex "00")
                 "11"))
    (is (string= (base58-encode/hex "0000")
                 "111"))
    (is (string= (base58-encode/hex "000000")
                 "11111"))
    (is (string= (base58-encode/hex "00000000")
                 "111111"))
    (is (string= (base58-encode/hex "0000000000")
                 "1111111"))
    (is (string= (base58-encode/hex "000000000000")
                 "111111111"))
    (is (string= (base58-encode/hex "00000000000000")
                 "1111111111"))
    (is (string= (base58-encode/hex "0000000000000000")
                 "11111111111"))
    (is (string= (base58-encode/hex "000000000000000000")
                 "1111111111111"))
    (is (string= (base58-encode/hex "00000000000000000000")
                 "11111111111111"))
    (is (string= (base58-encode/hex "0000000000000000000000")
                 "1111111111111111"))
    (is (string= (base58-encode/hex "000000000000000000000000")
                 "11111111111111111"))
    (is (string= (base58-encode/hex "00000000000000000000000000")
                 "111111111111111111"))
    (is (string= (base58-encode/hex "0000000000000000000000000000")
                 "11111111111111111111"))
    (is (string= (base58-encode/hex "000000000000000000000000000000")
                 "111111111111111111111"))
    (is (string= (base58-encode/hex "00000000000000000000000000000000")
                 "1111111111111111111111"))
    (is (string= (base58-encode/hex "06156013762879f7ffffffffff")
                 "22222222222VtB5VXc"))))

(test base58-decode
  (flet ((base58-decode/hex (data)
           (bytes->hex-string (base58-decode data))))
    (is (string-equal (base58-decode/hex "")
                      ""))
    (is (string-equal (base58-decode/hex "5Q")
                      "ff"))
    (is (string-equal (base58-decode/hex "LUv")
                      "ffff"))
    (is (string-equal (base58-decode/hex "2UzHL")
                      "ffffff"))
    (is (string-equal (base58-decode/hex "7YXq9G")
                      "ffffffff"))
    (is (string-equal (base58-decode/hex "VtB5VXc")
                      "ffffffffff"))
    (is (string-equal (base58-decode/hex "3CUsUpv9t")
                      "ffffffffffff"))
    (is (string-equal (base58-decode/hex "Ahg1opVcGW")
                      "ffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQ")
                      "ffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQ5Q")
                      "ffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQLUv")
                      "ffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQ2UzHL")
                      "ffffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQ7YXq9G")
                      "ffffffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQVtB5VXc")
                      "ffffffffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQ3CUsUpv9t")
                      "ffffffffffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQAhg1opVcGW")
                      "ffffffffffffffffffffffffffffff"))
    (is (string-equal (base58-decode/hex "jpXCZedGfVQjpXCZedGfVQ")
                      "ffffffffffffffffffffffffffffffff"))))
