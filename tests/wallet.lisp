;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools/tests)


(def-suite wallet-tests
  :description "Unit tests for wallet functions."
  :in monero-tools-tests)

(in-suite wallet-tests)

#|
Info about wallet-1
-------------------
file: tests/wallet-1.keys
password: 123456
mnemonic seed language: english
mnemonic seed: dedicated dubbed coexist having damp ember feline inquest september nobody alley binocular lopped moat agreed wayside gotten bays layout nail vixen imagine weird yahoo moat
address: 43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ
secret spend key: f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306
public spend key: 2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49
secret view key: 777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b
public view key: 8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629
testnet: no

Info about wallet-2
-------------------
file: tests/wallet-2.keys
password: bhunjivgy
mnemonic seed language: french
mnemonic seed: brave visuel version tango davantage baobab quinze essai peau tellement balcon brevet tasse ordinaire rhume lueur oreille version stock agonie salon scoop rouge seuil peau
address: 43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7
secret spend key: b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a
public spend key: 3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085
secret view key: 9197ed5871e7306e49f9d604dc3d9c64aa678bb0736b80f09158ab5ce9305e0c
public view key: d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3
testnet: no
|#

(test get-wallet-keys
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-1.keys"))
         (password "123456")
         (keys (get-wallet-keys file password :chacha8 t)))
    (is (string-equal "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                      (bytes->hex-string (geta keys :secret-spend-key))))
    (is (string-equal "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"
                      (bytes->hex-string (geta keys :secret-view-key))))
    (is (string-equal "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629"
                      (bytes->hex-string (geta keys :public-view-key)))))
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-2.keys"))
         (password "bhunjivgy")
         (keys (get-wallet-keys file password)))
    (is (string-equal "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a"
                      (bytes->hex-string (geta keys :secret-spend-key))))
    (is (string-equal "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "9197ed5871e7306e49f9d604dc3d9c64aa678bb0736b80f09158ab5ce9305e0c"
                      (bytes->hex-string (geta keys :secret-view-key))))
    (is (string-equal "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3"
                      (bytes->hex-string (geta keys :public-view-key))))))

#+sbcl
(test bruteforce-wallet-keys
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-1.keys"))
         (keys (bruteforce-wallet-keys file
                                       :threads 4
                                       :characters "0123456789"
                                       :minimum-length 5
                                       :maximum-length 6
                                       :prefix "12"
                                       :suffix "56"
                                       :chacha8 t)))
    (is (string= "123456"
                 (geta keys :password)))
    (is (string-equal "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                      (bytes->hex-string (geta keys :secret-spend-key)))))
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-1.keys"))
         (keys (bruteforce-wallet-keys file
                                       :threads 2
                                       :characters "abcdefgh"
                                       :minimum-length 5
                                       :maximum-length 6
                                       :prefix "12"
                                       :suffix "56"
                                       :chacha8 t)))
    (is (null keys)))
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-1.keys"))
         (dictionary (asdf:system-relative-pathname "monero-tools/tests" "tests/dictionary.txt"))
         (keys (bruteforce-wallet-keys file :threads 4 :dictionary-file dictionary)))
    (is (null keys)))
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-2.keys"))
         (keys (bruteforce-wallet-keys file
                                       :minimum-length 9
                                       :maximum-length 9
                                       :prefix "bhunjivg")))
    (is (string= "bhunjivgy"
                 (geta keys :password)))
    (is (string-equal "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a"
                      (bytes->hex-string (geta keys :secret-spend-key)))))
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-2.keys"))
         (dictionary (asdf:system-relative-pathname "monero-tools/tests" "tests/dictionary.txt"))
         (keys (bruteforce-wallet-keys file :threads 4 :dictionary-file dictionary)))
    (is (string= "bhunjivgy"
                 (geta keys :password)))
    (is (string-equal "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a"
                      (bytes->hex-string (geta keys :secret-spend-key))))))

(test encode-address
  (flet ((encode-address/hex (public-spend-key public-view-key)
           (monero-tools::encode-address (hex-string->bytes public-spend-key)
                                         (hex-string->bytes public-view-key))))
    (is (string= "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"
                 (encode-address/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                     "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629")))
    (is (string= "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"
                 (encode-address/hex "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                                     "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3")))))

(test decode-address
  (let ((keys (decode-address "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ")))
    (is (string-equal "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629"
                      (bytes->hex-string (geta keys :public-view-key)))))
  (let ((keys (decode-address "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7")))
    (is (string-equal "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3"
                      (bytes->hex-string (geta keys :public-view-key))))))

(test pubic-keys->address
  (flet ((public-keys->address/hex (public-spend-key public-view-key)
           (public-keys->address (hex-string->bytes public-spend-key)
                                 (hex-string->bytes public-view-key))))
    (is (string= "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"
                 (public-keys->address/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                           "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629")))
    (is (string= "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"
                 (public-keys->address/hex "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                                           "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3")))))

(test secret-spend-key->address
  (flet ((secret-spend-key->address/hex (secret-spend-key)
           (secret-spend-key->address (hex-string->bytes secret-spend-key))))
    (is (string= "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"
                 (secret-spend-key->address/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306")))
    (is (string= "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"
                 (secret-spend-key->address/hex "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a")))))

(test encode-subaddress
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 0 1))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey)))
    (is (string= "841P6Q51rJJcTR9PgCFNtP6gq2oAJpKtiNh9hQ9AcJ8jabzZRpy8acqHoriBmqCFBPVRBsAseVBmfduEj3Ys2FyGDEpQZ7C"
                 (public-keys->address public-spend-subkey public-view-subkey :subaddress t))))
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 1 0))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey)))
    (is (string= "82WabaG7VAXJsdWGkiUJ3u7LkvQ5aR8rqhr1zBWdwdZN2RbjHLYi6mVAZqjucfX5soBngKiQLMt5FFE4VrXA9tE1Lv2ahZf"
                 (public-keys->address public-spend-subkey public-view-subkey :subaddress t))))
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 1 1))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey)))
    (is (string= "876NP7hhudXT5wkGMzD2AA58gpzPtcJwDfUYoRijK54iBgNTha9uhQj5jNCHLwQfRjdi4V8RpAeTBUJptbe3FK3V49mr3CW"
                 (public-keys->address public-spend-subkey public-view-subkey :subaddress t)))))

(test decode-subaddress
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 0 1))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey))
         (keys (decode-address "841P6Q51rJJcTR9PgCFNtP6gq2oAJpKtiNh9hQ9AcJ8jabzZRpy8acqHoriBmqCFBPVRBsAseVBmfduEj3Ys2FyGDEpQZ7C")))
    (is-true (geta keys :subaddress))
    (is (equalp public-spend-subkey (geta keys :public-spend-key)))
    (is (equalp public-view-subkey (geta keys :public-view-key))))
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 1 0))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey))
         (keys (decode-address "82WabaG7VAXJsdWGkiUJ3u7LkvQ5aR8rqhr1zBWdwdZN2RbjHLYi6mVAZqjucfX5soBngKiQLMt5FFE4VrXA9tE1Lv2ahZf")))
    (is-true (geta keys :subaddress))
    (is (equalp public-spend-subkey (geta keys :public-spend-key)))
    (is (equalp public-view-subkey (geta keys :public-view-key))))
  (let* ((public-spend-key (hex-string->bytes "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"))
         (secret-view-key (hex-string->bytes "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"))
         (public-spend-subkey (derive-public-spend-subkey secret-view-key public-spend-key 1 1))
         (public-view-subkey (public-spend-subkey->public-view-subkey secret-view-key public-spend-subkey))
         (keys (decode-address "876NP7hhudXT5wkGMzD2AA58gpzPtcJwDfUYoRijK54iBgNTha9uhQj5jNCHLwQfRjdi4V8RpAeTBUJptbe3FK3V49mr3CW")))
    (is-true (geta keys :subaddress))
    (is (equalp public-spend-subkey (geta keys :public-spend-key)))
    (is (equalp public-view-subkey (geta keys :public-view-key)))))

(test public-keys->subaddress
  (flet ((public-keys->subaddress/hex (public-spend-key secret-view-key major-index minor-index &key chain)
           (public-keys->subaddress (hex-string->bytes public-spend-key)
                                    (hex-string->bytes secret-view-key)
                                    major-index
                                    minor-index
                                    :chain chain)))
    (is (string= "841P6Q51rJJcTR9PgCFNtP6gq2oAJpKtiNh9hQ9AcJ8jabzZRpy8acqHoriBmqCFBPVRBsAseVBmfduEj3Ys2FyGDEpQZ7C"
                 (public-keys->subaddress/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                              "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"
                                              0 1)))
    (is (string= "82WabaG7VAXJsdWGkiUJ3u7LkvQ5aR8rqhr1zBWdwdZN2RbjHLYi6mVAZqjucfX5soBngKiQLMt5FFE4VrXA9tE1Lv2ahZf"
                 (public-keys->subaddress/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                              "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"
                                              1 0)))
    (is (string= "876NP7hhudXT5wkGMzD2AA58gpzPtcJwDfUYoRijK54iBgNTha9uhQj5jNCHLwQfRjdi4V8RpAeTBUJptbe3FK3V49mr3CW"
                 (public-keys->subaddress/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                              "777cd85ce14d5be04de46f348144e78fdccc79fb0599864f867cb02ce389450b"
                                              1 1)))))

(test secret-spend-key->subaddress
  (flet ((secret-spend-key->subaddress/hex (secret-spend-key major-index minor-index &key chain)
           (secret-spend-key->subaddress (hex-string->bytes secret-spend-key)
                                         major-index
                                         minor-index
                                         :chain chain)))
    (is (string= "841P6Q51rJJcTR9PgCFNtP6gq2oAJpKtiNh9hQ9AcJ8jabzZRpy8acqHoriBmqCFBPVRBsAseVBmfduEj3Ys2FyGDEpQZ7C"
                 (secret-spend-key->subaddress/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                                                   0 1)))
    (is (string= "82WabaG7VAXJsdWGkiUJ3u7LkvQ5aR8rqhr1zBWdwdZN2RbjHLYi6mVAZqjucfX5soBngKiQLMt5FFE4VrXA9tE1Lv2ahZf"
                 (secret-spend-key->subaddress/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                                                   1 0)))
    (is (string= "876NP7hhudXT5wkGMzD2AA58gpzPtcJwDfUYoRijK54iBgNTha9uhQj5jNCHLwQfRjdi4V8RpAeTBUJptbe3FK3V49mr3CW"
                 (secret-spend-key->subaddress/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                                                   1 1)))))

(test valid-message-signature-p
  (let ((address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ"))
    (is-true (valid-message-signature-p "Abcdefg" address "SigV1d75gTEYVHbHYr6WH9LxDEvVRafCL6va1tTnqJhZXXhnhUau6cfijjTF68ntbjZE94uHA3odbfGuviHaY3hwHmWQc"))
    (is-true (valid-message-signature-p "This is a message" address "SigV1VsTCdRMjsTKDRcFEBdEcn9EMMuwCqavDr1XtyhTE53mW2Wc6yZyAag5epx68yAq3LibpLQocmAXZyMa6yewZdDjv"))
    (is-true (valid-message-signature-p "a1b2c3d4e5f6g7h8i9" address "SigV164c36GEuQRYEg3RwEBD4wtLi7yPqi8i3a38BQAJvUupod51m38URzX14simDHYTC5EdUK9ePB51T4UEaSJwVBwCX"))
    (is-true (valid-message-signature-p "Private Digital Currency" address "SigV1QV7kHxJPjVQQqqD192Z6BBF2AxaknFNTxYDc5RXhjFdHGYdHpEigkM45FxdmXTJ537aXSKDAvNP3ZeXRETKmur5U"))
    (is-true (valid-message-signature-p "Monero is a secure, private, untraceable currency." address "SigV1a47xuvk2AwvCVZJNbLtjNiXSg8kMyezrLTck5Jufpa57HmpSiPcZqJKCbUUw4QB2MaWRqkwKvJ3nLXQ8fCDSdVCq"))))

(test sign-message
  (let ((secret-spend-key (hex-string->bytes "d551999169b794459b4c8dc7da177067213a0eb2dd75cacf19e0fbc27dfc320e"))
        (address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ"))
    (dotimes (i 10)
      (let ((message (base58-encode (ironclad:random-data 100))))
        (is-true (valid-message-signature-p message address (sign-message message secret-spend-key)))))))

(test valid-file-signature-p
  (let ((address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ")
        (file-1 (asdf:system-relative-pathname "monero-tools/tests" "tests/message-1.dat"))
        (file-2 (asdf:system-relative-pathname "monero-tools/tests" "tests/message-2.dat")))
    (is-true (valid-file-signature-p file-1 address "SigV1Nh4zdYvLpH4Q8Mgi9CAnf5FgK5ExjEmS1S8cQVAt4Qgs8Jx4GohCV9z8qSgfK1CzymMsCYuBELjjkLGUa6n6tBit"))
    (is-true (valid-file-signature-p file-2 address "SigV1bcxztfKHU3hRaYrEiWjh3s5KCzDwL7bWKDTWzger6dSUD8pSjtkn7G8SpWjK7obDyJMe7JEx7okYYe23JFXWDhm6"))))

(test sign-file
  (let ((secret-spend-key (hex-string->bytes "d551999169b794459b4c8dc7da177067213a0eb2dd75cacf19e0fbc27dfc320e"))
        (address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ")
        (file-1 (asdf:system-relative-pathname "monero-tools/tests" "tests/message-1.dat"))
        (file-2 (asdf:system-relative-pathname "monero-tools/tests" "tests/message-2.dat")))
    (is-true (valid-file-signature-p file-1 address (sign-file file-1 secret-spend-key)))
    (is-true (valid-file-signature-p file-2 address (sign-file file-2 secret-spend-key)))))

(test valid-payment-proof-p
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (transaction-hash "8d245fc820dac077a32db250074c50f995f93630bd374ba3566f5e1d3fde3d4a")
        (transaction-public-key "5dd5e0faabe08ccf904a45e486d19ae8e67cb5a17e7e03104070dce80dd26f08")
        (proof "ProofV1XnFfDiZjLCDGPYhB26pLUr5yKxhmpAFP1gjM7z58GBpaN5USEd5EfVd272Dogct48o69HnwiyKJspBDDCDV6KzuZeeSzFXQFPhhe5Hq9mAGjqp6XTDTXK6TKWTD4zNi2jT6y"))
    (is-true (valid-payment-proof-p (hex-string->bytes transaction-hash)
                                    address
                                    (hex-string->bytes transaction-public-key)
                                    proof))))

(test encrypt-payment-id
  (let ((payment-id "1122334455667788")
        (public-view-key "68c55751c35ec5347064aac50c5368e6f9dea74cb540646f762c39bf4044b804")
        (transaction-secret-key "e00408cc0e3f5b435c1e10c70757ace784d3f732b268ec94ba7d48388c223e0b"))
    (is (equalp "833ddb809b9c21dd"
                (bytes->hex-string (encrypt-payment-id (hex-string->bytes payment-id)
                                                       (hex-string->bytes public-view-key)
                                                       (hex-string->bytes transaction-secret-key)))))))

(test decrypt-payment-id
  (let ((encrypted-payment-id "833ddb809b9c21dd")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
        (transaction-public-key "c0156f605aa27e302041a2f18a1954ce7cf4535211a5beaa5d6b9fd55317c850"))
    (is (equalp "1122334455667788"
                (bytes->hex-string (decrypt-payment-id (hex-string->bytes encrypted-payment-id)
                                                       (hex-string->bytes transaction-public-key)
                                                       (hex-string->bytes secret-view-key)))))))

(test output-for-address-p
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
        (output-key "49b37a01ae9f4864776fef678bdbdbde0d07825f70e9e0c45e02d38fc1e615d7")
        (output-index 0)
        (transaction-public-key "cebcf16a2a1e44a3204f502a6a979ba0d77b1be39859cab283df052c8f99da99"))
    (is-true (output-for-address-p (hex-string->bytes output-key)
                                   output-index
                                   (hex-string->bytes transaction-public-key)
                                   address
                                   (hex-string->bytes secret-view-key)))))

(test decrypt-amount
  (let ((encrypted-amount "3d09a2bf7867c9b90be6789f2a694b854fb8f014a4cb347ab7996389516c4d00")
        (output-index 0)
        (transaction-public-key "5dd5e0faabe08ccf904a45e486d19ae8e67cb5a17e7e03104070dce80dd26f08")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00"))
    (is (= 3783303208739
           (decrypt-amount (hex-string->bytes encrypted-amount)
                           output-index
                           (hex-string->bytes transaction-public-key)
                           (hex-string->bytes secret-view-key)))))
  (let ((encrypted-amount "5474d0b5fe10b407a880713ed78f2118e9928b2a170fe0878e0671c3d860ae0e")
        (output-index 1)
        (transaction-public-key "5dd5e0faabe08ccf904a45e486d19ae8e67cb5a17e7e03104070dce80dd26f08")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00"))
    (is (= 30000000000000
           (decrypt-amount (hex-string->bytes encrypted-amount)
                           output-index
                           (hex-string->bytes transaction-public-key)
                           (hex-string->bytes secret-view-key))))))

(test prove-payment
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (transaction-hash "8d245fc820dac077a32db250074c50f995f93630bd374ba3566f5e1d3fde3d4a")
        (transaction-public-key "5dd5e0faabe08ccf904a45e486d19ae8e67cb5a17e7e03104070dce80dd26f08")
        (transaction-secret-key "5404868a109869804a2e45f6797c250e68d9c5afe9cb9c1fd1ed8108082ac909"))
    (is-true (valid-payment-proof-p (hex-string->bytes transaction-hash)
                                    address
                                    (hex-string->bytes transaction-public-key)
                                    (prove-payment (hex-string->bytes transaction-hash)
                                                   address
                                                   (hex-string->bytes transaction-secret-key))))))

(test valid-inbound-transaction-proof-p
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (transaction-hash "ae4671480fa0c139bfeea2913210dcb8130ae3eef688a707d91923528fee1c14")
        (message "message")
        (transaction-public-key "cebcf16a2a1e44a3204f502a6a979ba0d77b1be39859cab283df052c8f99da99")
        (proof "InProofV11MFcTEcRh6rZf9boPNaShJDfYwGVeMNSjfNz9zFh8aVrBHBA94GbBcq35o2cVj1cUFEsAZ1HL59XK3PQtpgUePps6654CweyYaYKjVqhG7MAxDTa3A7Wx9T3gM3siXYrgFj2"))
    (is-true (valid-inbound-transaction-proof-p (hex-string->bytes transaction-hash)
                                                address
                                                (string->bytes message)
                                                (hex-string->bytes transaction-public-key)
                                                proof))))

(test prove-inbound-transaction
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (transaction-hash "ae4671480fa0c139bfeea2913210dcb8130ae3eef688a707d91923528fee1c14")
         (message "message")
         (transaction-public-key "cebcf16a2a1e44a3204f502a6a979ba0d77b1be39859cab283df052c8f99da99")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (proof (prove-inbound-transaction (hex-string->bytes transaction-hash)
                                           address
                                           (string->bytes message)
                                           (hex-string->bytes transaction-public-key)
                                           (hex-string->bytes secret-view-key))))
    (is-true (valid-inbound-transaction-proof-p (hex-string->bytes transaction-hash)
                                                address
                                                (string->bytes message)
                                                (hex-string->bytes transaction-public-key)
                                                proof))))

(test valid-outbound-transaction-proof-p
  (let ((address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ")
        (transaction-hash "6761c0560ba5fab0ce37b51782cc6e03cd1f3ed8442faba726b444594b384baf")
        (message "message")
        (transaction-public-key "fe34e8abd3d1c39686f25f8b5975fa160ae77fda33e3a6183007cc4abaf5ec36")
        (proof "OutProofV12CsdHk19Joa4cWq2Romg32T9pRrMKYzEBK73rais2m9xKoqPftMzMCqP51pAd6nCJjKH7faHiTTJPL3r4VgpszVGdmLz5eCykuWJ1KfSCsgFbCfHuqDYykSN7YMgFYSZ9BXw"))
    (is-true (valid-outbound-transaction-proof-p (hex-string->bytes transaction-hash)
                                                 address
                                                 (string->bytes message)
                                                 (hex-string->bytes transaction-public-key)
                                                 proof))))

(test prove-outbound-transaction
  (let* ((address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ")
         (transaction-hash "6761c0560ba5fab0ce37b51782cc6e03cd1f3ed8442faba726b444594b384baf")
         (message "message")
         (transaction-public-key "fe34e8abd3d1c39686f25f8b5975fa160ae77fda33e3a6183007cc4abaf5ec36")
         (transaction-secret-key "6f7b540ee4423231136603e4918aa8afcf96eda25156d3d2e96519a9406e0a0c")
         (proof (prove-outbound-transaction (hex-string->bytes transaction-hash)
                                            address
                                            (string->bytes message)
                                            (hex-string->bytes transaction-secret-key))))
    (is-true (valid-outbound-transaction-proof-p (hex-string->bytes transaction-hash)
                                                 address
                                                 (string->bytes message)
                                                 (hex-string->bytes transaction-public-key)
                                                 proof))))

(test make-uri
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (payment-id (hex-string->bytes "8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5"))
        (recipient-name "Alice MONERO")
        (amount (read-float "12.3456789012"))
        (description "A payment to Alice"))
    (is (string= "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao"
                 (make-uri address)))
    (is (string= "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5"
                 (make-uri address :payment-id payment-id)))
    (is (string= "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&tx_amount=12.3456789012"
                 (make-uri address :payment-id payment-id
                                   :amount amount)))
    (is (string= "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&tx_amount=12.3456789012&tx_description=A%20payment%20to%20Alice"
                 (make-uri address :payment-id payment-id
                                   :amount amount
                                   :description description)))
    (is (string= "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&recipient_name=Alice%20MONERO&tx_amount=12.3456789012&tx_description=A%20payment%20to%20Alice"
                 (make-uri address :payment-id payment-id
                                   :amount amount
                                   :recipient-name recipient-name
                                   :description description)))))

(test decode-uri
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (payment-id "8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5")
        (recipient-name "Alice MONERO")
        (amount (read-float "12.3456789012"))
        (description "A payment to Alice"))
    (let ((result (decode-uri "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")))
      (is (string= address (geta result :address))))
    (let ((result (decode-uri "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5")))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id)))))
    (let ((result (decode-uri "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&tx_amount=12.3456789012")))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id))))
      (is (= amount (geta result :amount))))
    (let ((result (decode-uri "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&tx_amount=12.3456789012&tx_description=A%20payment%20to%20Alice")))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id))))
      (is (= amount (geta result :amount)))
      (is (string= description (geta result :description))))
    (let ((result (decode-uri "monero:9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao?tx_payment_id=8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5&tx_amount=12.3456789012&tx_description=A%20payment%20to%20Alice&recipient_name=Alice%20MONERO")))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id))))
      (is (string= recipient-name (geta result :recipient-name)))
      (is (= amount (geta result :amount)))
      (is (string= description (geta result :description))))))

(test decode-qr-code
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (payment-id "8a17098fde6a8fa13087089bcddf8d49d2a8522c15870ab0a07ce4b162d4a7d5")
        (recipient-name "Alice MONERO")
        (description "A payment to Alice")
        (file1 (asdf:system-relative-pathname "monero-tools/tests" "tests/qr1.png"))
        (file2 (asdf:system-relative-pathname "monero-tools/tests" "tests/qr2.png"))
        (file3 (asdf:system-relative-pathname "monero-tools/tests" "tests/qr3.png"))
        (file4 (asdf:system-relative-pathname "monero-tools/tests" "tests/qr4.png")))
    (let ((amount 3)
          (result (decode-qr-code file1)))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id))))
      (is (= amount (geta result :amount))))
    (let ((amount (read-float "12.3456789012"))
          (result (decode-qr-code file2)))
      (is (string= address (geta result :address)))
      (is (string= payment-id (bytes->hex-string (geta result :payment-id))))
      (is (string= recipient-name (geta result :recipient-name)))
      (is (= amount (geta result :amount)))
      (is (string= description (geta result :description))))
    (is (equalp (decode-qr-code file2) (decode-qr-code file3)))
    (is (equalp (decode-qr-code file2) (decode-qr-code file4)))))

(test make-qr-code
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (recipient-name "Alice MONERO")
        (description "A payment to Alice")
        (file (asdf:system-relative-pathname "monero-tools/tests" "tests/qr0.png")))
    (dotimes (i 3)
      (let ((amount (ironclad:strong-random 100))
            (payment-id (ironclad:random-data 32)))
        (make-qr-code file address
                      :payment-id payment-id
                      :amount amount
                      :recipient-name recipient-name
                      :description description)
        (let ((result (decode-qr-code file)))
          (is (string= address (geta result :address)))
          (is (equalp payment-id (geta result :payment-id)))
          (is (string= recipient-name (geta result :recipient-name)))
          (is (= amount (geta result :amount)))
          (is (string= description (geta result :description))))))
    (uiop:delete-file-if-exists file)))
