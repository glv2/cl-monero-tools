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

(test valid-address-p
      (is-true (valid-address-p "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"))
      (is-true (valid-address-p "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"))
      (is-false (valid-address-p "43oErH6q2FfVkVBXrkQt3yfYm8ZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"))
      (is-false (valid-address-p "4BJLvWCVPaHedc26qdvwKWiS638QyHggYgSgX9kKagYR1oGSEH9e46ch4dwe25wDbii7st4VMp2m5ENrtqe1f83E44K8zwK"))
      (is-false (valid-address-p "4AeYLx84FNuWTMyA9ssqUWRW9FDQjTp65KLguGrwNLk9P2SmuWh8cx73BkvwKFpCFXhFeCEvRti8R2fAL1jZDFEnV5VgDzA")))

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
                                   nil
                                   address
                                   (hex-string->bytes secret-view-key))))
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (subaddress "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
        (output-key-0 "b859088a9a09c0ff760b399282510b20a9c53069efc725627b6bc1ba9648d7ad")
        (output-key-1 "563ddb372ed3863a841f9300f025b77ea6dc35dc3b677951d36b0bbc4e90bab1")
        (transaction-public-key "70f715b5d256790f9625676a1319d4a2030f3294f34d5306277b12f4d83072ec"))
    (is-true (output-for-address-p (hex-string->bytes output-key-0)
                                   0
                                   (hex-string->bytes transaction-public-key)
                                   nil
                                   subaddress
                                   (hex-string->bytes secret-view-key)))
    (is-true (output-for-address-p (hex-string->bytes output-key-1)
                                   1
                                   (hex-string->bytes transaction-public-key)
                                   nil
                                   address
                                   (hex-string->bytes secret-view-key))))
  (let ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
        (subaddress-1 "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
        (subaddress-2 "BfZQpPkqnQkTJCmge6mpLjdQ4DRsjd2MiTAnXEywr8XRR1NbDoKgKqsFsUDZrABqJv6TZrQKnMwfTJApGMoHREXLRJJsLHT")
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
        (output-key-0 "18ce6685a1d0c85ca59bbe4053f0693ecd9c2b8fd0de4b33d0056b6d65962fb0")
        (output-key-1 "a95289fcab4c8f962154dab54f14806f3e0d4097884ea71eb5ee1e1bb03bc7cb")
        (output-key-2 "c28e0c02e533d7d4b7605900d51f7824d186ec870f794eb0fdcd89daf3af5ca4")
        (transaction-public-key "4bc8b6926a1676c657e0ddc97910676ec5aa456fddda2b20b3f6e9a728dc7914")
        (additional-public-keys (map 'vector
                                     #'hex-string->bytes
                                     #("274d8feba5237579c6e39294aed39ed00ffc46b34e4f9a9b50056123f10f4b0a"
                                       "fcfc267e421edaa27f85a811fa08232b1fd6fbd683938857467cdaefb756327d"
                                       "3c300a6a9852f2ac7d0b6264076e57f7df9825c34fcea52e6346cba9a9c2405a"))))
    (is-true (output-for-address-p (hex-string->bytes output-key-0)
                                   0
                                   (hex-string->bytes transaction-public-key)
                                   additional-public-keys
                                   subaddress-1
                                   (hex-string->bytes secret-view-key)))
    (is-true (output-for-address-p (hex-string->bytes output-key-1)
                                   1
                                   (hex-string->bytes transaction-public-key)
                                   additional-public-keys
                                   subaddress-2
                                   (hex-string->bytes secret-view-key)))
    (is-true (output-for-address-p (hex-string->bytes output-key-2)
                                   2
                                   (hex-string->bytes transaction-public-key)
                                   additional-public-keys
                                   address
                                   (hex-string->bytes secret-view-key)))))

(test output-destination-address
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (address-info (decode-address address))
         (public-spend-key (geta address-info :public-spend-key))
         (chain (geta address-info :chain))
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (indexes-table (compute-subaddress-indexes-table (hex-string->bytes secret-view-key)
                                                          public-spend-key
                                                          5
                                                          10))
         (output-key "49b37a01ae9f4864776fef678bdbdbde0d07825f70e9e0c45e02d38fc1e615d7")
         (output-index 0)
         (transaction-public-key "cebcf16a2a1e44a3204f502a6a979ba0d77b1be39859cab283df052c8f99da99"))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key)
                                    output-index
                                    (hex-string->bytes transaction-public-key)
                                    nil
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= address addr))
      (is (equalp '(0 0) indexes))))
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (address-info (decode-address address))
         (public-spend-key (geta address-info :public-spend-key))
         (chain (geta address-info :chain))
         (subaddress "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (indexes-table (compute-subaddress-indexes-table (hex-string->bytes secret-view-key)
                                                          public-spend-key
                                                          5
                                                          10))
         (output-key-0 "b859088a9a09c0ff760b399282510b20a9c53069efc725627b6bc1ba9648d7ad")
         (output-key-1 "563ddb372ed3863a841f9300f025b77ea6dc35dc3b677951d36b0bbc4e90bab1")
         (transaction-public-key "70f715b5d256790f9625676a1319d4a2030f3294f34d5306277b12f4d83072ec"))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key-0)
                                    0
                                    (hex-string->bytes transaction-public-key)
                                    nil
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= subaddress addr))
      (is (equalp '(1 0) indexes)))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key-1)
                                    1
                                    (hex-string->bytes transaction-public-key)
                                    nil
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= address addr))
      (is (equalp '(0 0) indexes))))
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (address-info (decode-address address))
         (public-spend-key (geta address-info :public-spend-key))
         (chain (geta address-info :chain))
         (subaddress-1 "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
         (subaddress-2 "BfZQpPkqnQkTJCmge6mpLjdQ4DRsjd2MiTAnXEywr8XRR1NbDoKgKqsFsUDZrABqJv6TZrQKnMwfTJApGMoHREXLRJJsLHT")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (indexes-table (compute-subaddress-indexes-table (hex-string->bytes secret-view-key)
                                                          public-spend-key
                                                          5
                                                          10))
         (output-key-0 "18ce6685a1d0c85ca59bbe4053f0693ecd9c2b8fd0de4b33d0056b6d65962fb0")
         (output-key-1 "a95289fcab4c8f962154dab54f14806f3e0d4097884ea71eb5ee1e1bb03bc7cb")
         (output-key-2 "c28e0c02e533d7d4b7605900d51f7824d186ec870f794eb0fdcd89daf3af5ca4")
         (transaction-public-key "4bc8b6926a1676c657e0ddc97910676ec5aa456fddda2b20b3f6e9a728dc7914")
         (additional-public-keys (map 'vector
                                      #'hex-string->bytes
                                      #("274d8feba5237579c6e39294aed39ed00ffc46b34e4f9a9b50056123f10f4b0a"
                                        "fcfc267e421edaa27f85a811fa08232b1fd6fbd683938857467cdaefb756327d"
                                        "3c300a6a9852f2ac7d0b6264076e57f7df9825c34fcea52e6346cba9a9c2405a"))))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key-0)
                                    0
                                    (hex-string->bytes transaction-public-key)
                                    additional-public-keys
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= subaddress-1 addr))
      (is (equalp '(1 0) indexes)))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key-1)
                                    1
                                    (hex-string->bytes transaction-public-key)
                                    additional-public-keys
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= subaddress-2 addr))
      (is (equalp '(1 1) indexes)))
    (multiple-value-bind (addr indexes)
        (output-destination-address (hex-string->bytes output-key-2)
                                    2
                                    (hex-string->bytes transaction-public-key)
                                    additional-public-keys
                                    indexes-table
                                    (hex-string->bytes secret-view-key)
                                    :chain chain)
      (is (string= address addr))
      (is (equalp '(0 0) indexes)))))

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
                           (hex-string->bytes secret-view-key)))))
  (let ((encrypted-amount-0 "5b8b146d95fab3372a0491eee3aa488d896ea917fc4af064cf1606e9c8dec509")
        (encrypted-amount-1 "dced18217899c0f4f652a427563ccbbdbbe14b5b649dc7d1749cec22d6c3c208")
        (encrypted-amount-2 "a61e4526d0874608bf7dc58f42a89fb93294931f270cb6d5d1c4820b6a3e8f0f")
        (transaction-public-key "4bc8b6926a1676c657e0ddc97910676ec5aa456fddda2b20b3f6e9a728dc7914")
        (additional-public-keys (map 'vector
                                     #'hex-string->bytes
                                     #("274d8feba5237579c6e39294aed39ed00ffc46b34e4f9a9b50056123f10f4b0a"
                                       "fcfc267e421edaa27f85a811fa08232b1fd6fbd683938857467cdaefb756327d"
                                       "3c300a6a9852f2ac7d0b6264076e57f7df9825c34fcea52e6346cba9a9c2405a")))
        (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00"))
    (is (= 11000000000000
           (decrypt-amount (hex-string->bytes encrypted-amount-0)
                           0
                           (aref additional-public-keys 0) ; subaddress
                           (hex-string->bytes secret-view-key))))
    (is (= 17000000000000
           (decrypt-amount (hex-string->bytes encrypted-amount-1)
                           1
                           (aref additional-public-keys 1) ; subaddress
                           (hex-string->bytes secret-view-key))))
    (is (= 338997666200000
           (decrypt-amount (hex-string->bytes encrypted-amount-2)
                           2
                           (hex-string->bytes transaction-public-key) ; main address
                           (hex-string->bytes secret-view-key))))))

(test received-amount
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (transaction-data "02002902000794db04f61782940683d301badc03bc31e703fb62fb7526a7131772e632cd2762404dcc8cca08f264eaf6156c6021a0faede0020007f585078e3086b20972cd02038602f781a032db7bb03ead857331483e39bda1b9060d7ccad7f76ccb9eed5f7fed55020007e9c409feb801b88102aed802ba8f01d304b901f62fed4d3c3cc768c511ef1b81582f101b9c71f4fc7f88cfabbd4b2eba395ecb020007b2a905bf8c02f5d002e9d701b0c401a9c703cf03e7c97bedd5763cdf3aa8539553890cba3f0b65b133da1bd3692eb8723ba500a7020007868506caf804be72b61cc2db045ff202e62906f6e9f47c741d9e3f0a38ed994772c7178ff984b1c6ce7fa379c3cf3c05020007cecd0784e005cfb102fa8701c102aa012ee39c55bed0cc154f6a364945d2dd054938da15b6be4bfc7ac34b231f7829ac54020007e0a406984ae58e04e784029de103800a0adf52940f9a8503468dc271c30f8fb832a5e1ab6d5d7cb51d5b4a9b4a8c3543cc020007c5fd0ac062a6e002cca702a804058401db0297c3b68d9dd834879c313d6e4497dac55a8669f93b23e3bb327a246fa96d02000781f406de4181799ff601fdad04a198028602d2f006299aba84037bd43f4c0922116227952fed5551d9f4f475b4feff4d97c8020007e4b50797c104d58103946fd516c069b401c96fa0903cb111583622655429a292b28c4a1ee9e4b9bc6dc294b2e1e9e6ff6f020007b5fd0ae2c805f9238b01051e23be04a4d342baaec78a310f1e33ee8520eee718b341029cce97ea8bdecf7b16f402000792b607d581079a149d1acd8102e20368ba8fce2f272663ac2c6c3a6f14bb3b7897f0cdb40a529c8a652dbd81cdd7d2c9020007bda904b5e7058fab02bd8e03b75c8d3fc001b5c81e9c048d8729a86038dcec6dcb986d2d536203525f32d8bf0cb6e3d974ad0200078bc20feb64804288022c35d601b45c628a69af4bdcbb1e2153da4af00f2bcaa8ab5d67b515ba10eee6165fbdf6020007ddec06c49004c9b003e1a602da16642cb020e3668fb014d22f3379ac8ee445618b9d947f24db820861d893f08571021c020007b09613d8d00181262fd003110bad91a1c18463341c86f2fa5f7acc8cccb9126a8fb0aa99f44d29f47ce144bf91020007e6fb03dfdf02dba1049cec0525a50258abd75529d2172056f002e72c91b836cc0970f2ea2046fd26e9d1023bb0b35d01020007b9d7069976b3d308a648a4032033a845c6ecd033d0acec1d09fc09078d973402363206b8326be174443ef33a04f1020007d4eb09c291019d8d059911a54e65f201a61f703d23318831942e6f0a8a43ad08e78ebf8c26e95c79c281bbee0a561c7a020007fbb507d4e201a5d00186a00192e104de0224937425ae5dada4129ff0a5953c6c6387cf4777d002d4b905ef1f56a7ef88943b020007c2fd0af79c01b270ae2ae4b003ef062792a120d2bc003fd7522adb40b5ee9adac977c4df70728c4c3e919580cbbcc011020007a9a008b8dd02daed0580014a03477f471a33ce14837a2f184a2a97d2f147524174d891f0ac173869f300beefad11020007d4cd07e3c804c4fd03ba58212a5d7c2e5727e5edbec48f20cacab02a7e63e705ce32cfda588d97522354284f330b020007d0aa05938b02b0649d9605bf4ffeec02717018f3b108bde5c8d926c05eb0bb8ad4240b1a141f3adfc599b6602f5070e6c0020007b9a205c8e801ef2aba5ce7c90580d202913e6cc0b235326bcf4f97d22933f4d0d9197c2f9845c48529f49b45beed990d92de020007ecb507bfe102e4b7038570fca402ea08236a223b1b64a952e2b6a5b2e215835bd0314863c8272b05426a54fb1b0ffd3609020007c3b102d1c702f3bd01b9c604e49e05d64e1964013ddffa8846e54cf25cb2157349f855dad5d49842eda0bccbec417b27c20d020007a6b60790b00954da028f014acf01630f94402fa265da4790165dd798eb31e10d60d8e1bf72147a892a4bfcdd0a93020007f2b5078ffc02b49601c9fb01eac302a45eb10357c322fcd78599a40e438ce1d0b7a9192db92f41e051639d10dcccc41e7beeb8020007cbfd0af4f8028147e2ab02ee020e9301460588244c190f9a73b39d2d61e5664076fad82191d1d692d85d531053df3e370200078cc807ebd8018b05b18e03c251b15dbd1142178e7499de49844d9d738ff52643ade1c1875c05b637d4b6b162d2d5c8aeac020007e7bd08b5bf02851b9063f39d03b6d001d8033654e85e34a890fb9222c4e7656827114d990433947398317b23fd9f9481ce5e02000784b607c09002a78602bc7fa954b5c8039c0133333d53216c28f8ea14b25dca4264b040338d604d147c6e2b591a196f6bdb54020007f5b507d247ccbe01baf502d2b40486055c2a2ce2bc522018cbe36b84e89d424f5ca56fae609ca61c36c7dd2926fc8f705202000794fd0afecb04e639e946b91f5255242c5eb6d1c3088034e8923c7c1daa2c1ec4c155f7c9fd0553144de790be15c5020007cfc8059185028ad801f1b902ed29c0e901a1181e27a3022e2e88578c8a6bf5eed97f779f8eca3d2cbf25fcb5fa9ad8ddccb76e020007f2eb04e9e102f9b207e1e60190028902351d28f1e3800ccff6b6bc608a006a190266fb994f2cc150300c1ff3c9294df1c3020007a0b607d7ad049c8305d6019301db0207177e303d13103fd53946fc9f2e1f8aee96a728b1e09d721eb0d376c078c74a6602000792c8079bb10491ee04b4029801014916a07dfb01bf8df0f5f02261ab6a5c96432e00b7e0dbb4b7d18a468aed5528b10200078ec90489b40681d901e88f01baa801d117e9c1010c69f9a79c039af7a12114e8dd5474dc451f5e3b191ffbbe31a539741431fb180200079dfd0ac8cd04bc03de8a01eb10ed01cc0106f90adaa07cb0cbf4ee84fc1ae8124d49bee9d1f904860b8f75354cfe699922020002c4cf05b555ab4c5c49a98487e38a42a06a3a4455626bb870b5b550aa8824a3cc000232ab9fbd0edf8524ccee3683a547a28ee00b47f18ecfee210b09aba78500f4352c020901833ddb809b9c21dd01c0156f605aa27e302041a2f18a1954ce7cf4535211a5beaa5d6b9fd55317c85004b0928cb41fb0ed2c6974e170dd4c6000eb800f6d97e37ae3fafa067434037c1498904fb60df15116ee145aebe85459eb074dff45681c513e20ca21d8051e409fca503ba9030f9bb290a1c28015ddbb8bdbbf6ae4e83a45861dd85d9357122e7c59ad8bf90eb25a3420b4a93305713873ef37c83fba17b64efd01a6cc22eefb6a07b8769906afcb3eedbbbbd41777a5be85a82e7ee2485750a041df8d226d30d4e5003bf05a5abeb66fbd7caddb833994b1611ee80b1b5978e61e02e49d5b60f0894b636858b3b13d2602843ad562e15f68d8fbd07b738406f79fc61c2244abfeeb25c59150bd77266e4e43220624c54faf2dcb0aa460a6fac269f2bf20709e902b30bd48222847d74b9aceda5a5803716961f68399fd9f0e316798f81965262ba26bd629bf4066e0a1ca1ce3e9fdcf9b184c215a7bde0821944f0991099190051fed883b940408ddb4f9390790fc4583691df70ed43c960a0512ad812fae2ed4054b7f1e013115f092afdff52f5819a565f13e45b41de375c4cd1edd54f6d056323fb1f002061a5684e982d5881cc90e4ef7724f75fa61f8c965d2de2407193045e06f3124615ae9c9e210c7603073373444496ffd2e4bd76997a5744b14fcab1201784eb83bac7ef022bc4d7dd0a41c65aa9cae7a0782bcaf4fa716a1d25b54b3df877bc440fa80c7771d0a44032d7117502ddd968633fce8591de3599122d47fed08bd04b10a7f59d3fc8e2cb5a8c285a1bfc5b0e451028ecbf94659906d83c410ee460fc5d8e1075a9d22b7bf8cdfa9167252d2e556439bba8f0e24d1f960bd8b1a34d64b061d6014849ce9927e588d249f968e1dd29fabfdc9cfff7a804ad136f44abf6e0eef3e280735fb032fb5b09c2b4ee8510523e7535935340a19d0fd1fa2358430b1a8a20723a85ea8c1d8942d42de4626237235777c21286c76b48308c7b36b7c404f1502a08382d2000e4e77b483d6554b0b227ded33506bd5386dfda18d3e589c2ab44d3478010104281d2304659374c8bf1269ec2fc7ed8b388475522a24fa107cbd2d9f60014c5ce6510c6882c28915402d0bdc87bb85600a0b440aa848dd488095f620004d8caa411b837794696e6e38e3fdaa20b27d1afdca21c2c2c1b803c7e24d4dad0be6d4fd3aff84e84cf8adddd5425cd9791b883deee7e23924b80fb74ecb6ac830cc026fb1864d3bbf2b8ae88cddf9c6b9a3fa723bb5b3b6a223082ebbcbf6535b912a13452fcef5fe44417556d34683155382d4f096022af59c94dcb6f9ad704baeed0bbe2d0d57821fe3c32068a1c779f4120c212bab088ab701099f968e3d7aeb04fbf3976dc9039a8d6c62348ada646479ab86d506820f0d4c87a100eaa16f73a1a30d091c097ffc3d26ff8f398f7a47ec4d5b6858480c5731576d0545052ed9b67e912d44622c852bf753fa8db1cc9d47925d34e60dc6130d0a16acb1d99ab9f2356ea9d8b3f132c344c7f60e95d69f8a2a54e6c25eb06e0806834e5fea5de7d5d823427b5cb1f4ac63275627f9502b3ac5a3445c074cffc2b216e321e153870f38784f08388de30b3ac07e43a5e81dc9cfb29f710b2a52f0fa5d62b6cb06d32490935ff15895481f4e85440485b8e1ab76958b751fddcb87c0ad2a190254450c2799ce46a735fe84ff8ce03f6a1b4321179116b5c9f65785c46f82e785f386b1ca1f27fd8a97cdff4319b9370aae25d1b3e634e3aabf0b4f3bcdcd521b65aa2b2cd4587d6045ea3587cb0ad3b1dfb5bf6815f416d8698ddf6d063fce440c03a2726e8ee7b980f120de669f50970be6bb1ebb2dd052126b7667f7d3bd3ef69d5a1e661e1d38bec80bf4c004c5dc6aad5dc817937f693bace163b76e302be5214c347a49d71982b60d0dc3345b5e6be039eb11d8ddaa46ca1a4a627f6ef5effa13f2b769d9b798506ef6b2cfb2ffcdab2073d64c5123e2039c8f595174f7b302533828ac86ab07a7c1ab7584a9ee14aea55f0f1d3caa2b12abe47d6c9f3b434cfadb3ee6b1f1ffc0ac9035323989c568ed55d224f1279e0a71596d1f876231a99e8020d30596072da340d32cd9db097219e8bf3dc61d47c5a0f30506f7651e25bf26563beb312d32b315c617b62ff0fbf7467e543c7fa877ee170a53a9178a2f2a97e241f95af392ef1ed066d70fa4a7b0fa15a4afd58527bc760bca79132f3997d6b7442b925016287be4fe92046f8756640ab1896ef13fcb7f01b6916395181f4fd09fa4eef80fad9e628a585e12ad7412b80fa01c7c846b4e09c290292d27b476c478ad1e866f8b56603f49c3c9442b51abd150d13ae2d746043756f694a3e86d826173526041cfc0891cb531501d19326333a7f277aa3cbe0fc290e5f065769062376b9d10ad3dac852468904db1ffeee29c56148fc87ece04fd080f690977498fccbc48ab81369ecd2b5acc78136f1ada865b169d30fac50cd8b4973106c82f45803f8d25ed7e1d291b1cbbbef98b6db218cf9d1c422990090797c7a640e47ca120a5acaf4bbdcc977941b7c57cfe3ced90b7aecbd34869087066f804dada7ef43dd5df7aaaed1492a76da803e6b62ed70b8c0ce7ad4de403e3958cf6f58d0be25c9601f1c267d5415af6388d4799137cad74fba628480e006f906e26d1c16c2b7755afd94f06c9bf3b403053f633dff1d397d108cd5ff4074f5097869e55633984f1457f19c7affb1a449345844720dcbbc9010200316904a0e2d0138b485cfb8bbc1991bc89e785d7f9b4375602dbdee7b64b305a523a0279f45ddb41ea57c3e3fea9eeacd8e630abbcef87f61c89e3ea3f5892183f1b0913dbb4d8e1c6e3c86fc882c14ac11ace0dee2c772414815b0a004b409da83b030d75f4b67d8064805c5fcfe0012d59b44e9036bc701945aa81d8ff45ba152f09935241b07b4f5ab0779994648d4de9a2fb13a2a9c2745d9819f6b791fe45d303dccd07b889bce73a23508875e1d956d975e8e2115b90266c741013e1c7e43c0d2f55cad1b957ece17222c8dc0e6f22659a471221481d20127cdedfa965947601f6a5f3c927ac5e8f2cf55e8d8aca90398bf461077cb738b1767b258856b73602407eca6e476f6ecb840ce69834f708c50648a870bb4d0ab246d5f14eced87b0f843caf314e73fe45a1a30fa39ea4fff28624f5757dca5932ad1d7c46584f590b6d4a11a51dd1ec9c7fbb7dcb47c664ee9106ae237855c123de4713a3ac25620c764a84f1b9b511c10a04d769c6e57af0a2d81e1260bd25d9f260a7ca47c5260e9759a4296189e7e31e95a3ec86b043e734c4ad08e6a7487c2fa605486d3f6406ea8922f9083c7a44f75e889a66122e0a9eceac2477a2b0474efc7753f7dcf7035a4b69e96b269a10fc9a780a11afb5fa83a90f4cb17e34b23f304e0698b92f05f4400b82975c32fd80f7617148b733e2ca51cf4b9bb19564f33457f6b69ee4009dd634adb3b001f2054b232d13ab354e9ec301ce7ebd3c83760a3be6e904340fe109a5cc1d7268eb881b856dbafba23a3f66e66b69e9caba26f70351c7546e0d84db9d5ef14e5e01abd9accee41fd716a0de322414fed004b4487871b6565703941e56cdb9019998762360d710e4d4505ecaab49cec8862857ca8a73d27ab70691084220cc23014c3d6dda43482e77a86ec8bab58b4b1235fbd1ef2abf42eb0c6d49a285b1e9e00bec632ccf28e3b8a8e8cc13c37a966338db39a8f62878a2091319b9aa7d27fdbdbdf7feebf614aad5e029705f859741969c74570923b49a05a43b95feb5faf6100817880c6cc333c1b8ff8b7ff0f41b4205794018887c24096252a0ae9ec3d865ffa0e5c23754c6b9bbfc037d6f11ca1e7615dbaa2d84c407d62725d878b4086e5b44fd66da542bf3d1828e2576f2606ad9912afb35bb29086ddddd8b2cb78ae2f65d65fcad179efc43c458179af9b4b1a50e4a50e123b80aa5c274172f5d7b53581e3d10bef4cec53934fdd2930775edcf65eda401d4d200c32e1e592e1b92bc7211d29a5bcd4b24ac8477bbb806dcea11db66bce7d6fd07fb399be6f9d30cee1110e5990f2cb648a0fc31ff6e68242173291aecc1468d0551f693f950a27813eb42ace67a30a48ebcf291868518d096e7a36ce5d9aa7b0e61ea24ff900d866082470c31a265b9a08da81239fe92f7f5ab77380c371aa40d414f510dea8ea56d91177be3a369eac36b319d82866499b154ca61a30474bb091aff5ddc81f06d0737c439b8f8563ee4ab427e1a2ece8ff33b9d3718be91c505b22295bd807ae6dd5167181207b83588367332645174dc0285e24ba4f3042d06a229b64580c440f03695119efdb054bf63096605afda3226dde78d02e775df0a597973203ff538714797a0977f8d932c5e6a9a899cd2227a25cd864491026e0241c823f1a0e6b7ca8bd07f1d1913af7aa717dc2e13b5c1a5b01c74a97059ad029a3a155e27db38d854f00fe7c8aac13aff2f45e72a30776abea1a55dd4fa020ddd7185bfc81e9564f52a26ebc4c438c4d7545d3e6a0dfa96bc1a020755e7380440ca6f5617444da637f65e95a21612fe80ba823e4ad7a0abf0bf08911ad0bd026592d9a22c06cd4a2411bbb3a79d37b479f7a70c8c29191a2b5c5d269a16000be76ab2c1b7ee1082065eff73994f1292d3b4fcae5c46d2003c1b5f17b3dd300abe27e08c10269e7e62debae855d4425446348fa3708efcc15c1adf2ab60f1903b95b6fa3e32f72f0c52e41a9ea84f290168b7d04f3ba2ae3f1a1bcd2cc18d4078ae1b9d4e03a16d7b3e73ed9d09f1706e0cf6ba6300d040bfadaa6c86a590d097a7bc8ebdaae2d870bcbcb46f0f452400e87bf1e855a8aaa9f2d9f5e757c0705c1d1baa92a287db53a703610e58034925b9225f69b6d4d836e07918ac47f830acfc52283d5e4f1157f9002f00c8a48690cc9d1b1d7f3620a2234f6a371783b066b2a86e4211782900009963d0ad46d5863865483130f061fc56ac2a2bc3a090bb687df8d886998fc3735fb507312fb6aa7d1bcd54e2d2ea85e2599695705f606256b41ed42584d6ffa53f99a602a517fd4cf07d70219eaa0ebd7e24ee91ff30de40a9cd04987ff31981103ee96bb2a4e617468307b64bea3e620a6777925960b1017416c68b7eaa9e179637de327afe03de1cf10886ba8220da9795a6481600ef1f7a357532733e4d55fa295a891e67513fb8317c23da0279e4fed4b62fa5c09b28297c6b6bf00ec2335ed055ce3b1abe31f12e40f915e573da93472b878be005bc373350ba6949902c1e0a336ce74a8a5e1394e88ad4750a5139f6e4582db00f12eba918268abc503103177b5248e1a1af3049fd5bd1d954ada30c015010f05e3df94555112019d2ab266ebc6df23c3d9d605e46449d4b08d63a3c9b0f114045458b94c7fa2463349d39c0a7331e999898dee49973588666bd213b47d37c90ffa2dd75218ae830ef590d6701a57a4428c459b3e1ecf0059484c20db3b544900a267f53162f6a8990342236d3b47d025f214e9a19a99f4b4f66c167674f99007014a91b4572c1be6e2223c70a6c810b2b31080fae1e380bcbef6b2de7e8e6a0a14061e10c782cfcd926685eb00326b89ba26715b57da7c2ef31e13c600002d0e0da42751bf012e2dfc76fd2761cca657b2a81ed5cfe84c01eadfca43598dad01f93140c43ebb7c707495d6ade263342b0b8b29f0a2ed1d693e172b50887ff80db8a0a3defd342205c3f82d1adb6bf46916ab67224e263b303917b0d577a5900350fa288e00c931187f34eeb6fe23c13f61c6593c9b70591d05318e55304ec204a9116e1e37a4754d6551084d4473b56835c111fe216b98952e080eed3cb81b03bd178c66ffbebcd62686a5fef9bac03791bd6abc69b103c4195a8c2f4737e7038d88a5b277069ca8d872f35ea5752fff49f9304059872c439fb30018daa97504e877cf9dc8d7afb5d2f0cb2a09d5416375a4789e013ac996dd7043fe33dceb09babae651aeeafc854f674e13f7e5019599e3cc01c1942b377e75e3ff7c96a00a7399a10007e96643e12a0ff29458ce1f3ebd411a7cebe423e779f9cf1949e204ede3edf93539dd364b74bde44de8f9c2153f17d748c1ce2ee14a267717fdca0146f40ffef5a58074ae54301ce684184242aebcee305c897fb292b041e441d6055a72ffecc1bf6fc40ff163f4e451cad042fa633d83af6151ca860d5a832bb30b762886919b4b6fc680a92bfebf797ed294f99ecabe3d5267940bc4a172a948033497ea7d174e171471c901fd13b75c53543a25d84444c70dc94b13024b69250ab7bd733a87691729862c266f9ed8cb6dee45bbae782501f34ff187cd8f310307ded74b19b11c9dd6c8e6e18b82beb517e82715eb975842d9701eaabfd0dc70010a568dbd973e45f10b31f39a59cd02b2328096354e52905dd2ec6e6a6adaf50bc4b93471b0b93e1a6343562596dfac6b46e82173a851ccce054fec7056c88b00471afa64090b725716ca12a6f906d8d5632ca2714bb7e15b46a9f86205718c0264a34d7de41f0de6c31d0c38d27ed71722220c2def6f4afb150e0d8537be1304cbc0d57554c38b4891e2662a318ac45e35452bd89e04cc33c06dfa069467da0871178da13b2d78273d08541bf8a718e4dcfad8bbb05e3d0839627b1242114b0a5f414dd24cc48c1a3b6fe7d3e2cfea772535b7e33207ed8da8832caff5c9f907bd40e883769b2720577121b3a6038fe1ab175c5f560f4b6256f6f107fe450c0ae58d0d65809386f105e136ea5e93fa82dc2bfbb1a9757e185964715f3807480747954cc0b1192f674685713c224dfa3dba8664d7378ae8d7a8bf31f6ca6e2007b3d3aea1d9ccae30146a2012386d48af70edda5b5973d80a25ae3fa04430a9090454f270f9b81ee888c882dba767e57623e82bfaed8c381cc48ddec8f6480b0e5486c00568b02ec0355cc37e8ee120903d64470c269c9055ad089e941fc0a60bb148b5564d7052f1462d84f4a17b6daf2980011042cabe782b194dfc06df410e9522c0edc3c4f22eb27cab2f4d3679d477f1dd4b28378a718e5e2fda66548002ecb391e9565c5c0606d4e0683418a707cb8f3489f1359be8d7ccb970bb62d8031686935332a65d8f3434fc24431e894cbbac03e48086e6589f1a36ed0288d40971ff8ed09278ace79a45fb58bb92eafdc02f32d3603a4d6b55aa4e2b88675f064b654338cd40e117a3df3fc39c7a1504b24285e775227cc0f13c6b549a13600e18c91706873f5efd2c3a8f7b74fcc6136cf28329005e79934aa76baa899a190a2580f557a5536a9b7b2f72f0310629d1a3cc98517fe3c290af336cb7ead5d90c6afa76c3dd876b122d578dff53b65296ca470e0e6f8ea091afef7bd72ddd340a5a48586c9e238cd4b9b30170b3c3eb607b65138452776bb14ef7c7bdbbd78806eea4790f76bb756290e8376fbe61c6b6e604f3e885b872df628c63a18cfc6e0c1c890ec57f16b1fd67c861445d63e5c128fde39dd05854b5a9c441e337aa370af6acc33442dd706395b980d7fc9e6f3bd6cfc0297ddd7275fdbd90a4566d2b005152075602980ab49440e71e79ffe1e81ae4467e954e8e5507b6ce10a043c40001037a0050c8a10c4ebeaca8e2471af0ef2829ea21d7f4806d35b294810b2c0aa3fdcf6f76c95386422499c6502527e0d18e1acc1fc2b5881e2f6bc0aa5d770abf90030e5359e8b8bfc849dcda00bc071493ee68a7639acb8f41f0a528a470008722b775d15da01e76228e2f4f3d1b26865e43099fd8373ec64cb5ba7877750244be946b52d2a3b4d778a131aff75c5ce1400ded309ea1870f26aaf17fed3503cd12f7834ce89b616a0b0c35ff3dd2e486f5f2a285ed99e4908e8d97728d3f055f399e6248b37d480bbbae3234bd6d16a648027d986ef5fafa654db8d5203002d11ce0da7bfc04dd1ab44cc105001c8c515b0a299c6db5a2cb37179e373f3105131be55ea2018bfef998d3dd75df7ac63cbdbfda4c53afdc5c11f5d21416f9054b1a34e0614023d923150fce3d73d699e15f7490e850331269626e295945b80b48ad50a407342ae414036fbf809b0f16b2d68bf2b602226b14327f835c25460cdd5dc277013776190ea457c09e4fd0c6d62f57f5348fb5138e876189256ca4039b6be204f336d8287e84f156a465efd2393d2975ba8a19043e2d87a3df08950e510d36d908609fd68a704dd558da1676a0fee97c26e0555a966cd4af25dc730523d04acd298b950b02b0f3568e5d6c52e279303ccc8ddf82fe61898edb29b40bf71c42cd1d2c07e912b6c13db513daf17f5294319fc2d8c584b58da427e8e10fc18a7c18cca45f18cddc1c4a87d577b74eca054dc862abfc3a1786ee3171b3007758796068bb206de05cc392d8d3059bc1ea8d29004a577d441498570967430bb9fc903ca927c6b181d1cc20ed74038e3d8396e6bd0f7951f84ba311ac75f90fa1c1943ad5fbebba897c226064c82bf1250acbb9d2f9419aeb80310237a83c0130640ab28ab3fec685d2c2b0b0cfcfdee981d0745d4d6b383f0c79d1b324880bbc3870fc382f73c49ac3d349526cf689e7b3d3ecf7c4f525bb0c933e522b05082639c58a9470b358c53a2925f2717726868aafa85e8d6eba470e3389f7acd40c9dd35749a32ffcf028ee263bacaa071244b5fb40ae4898258c022767db41bd07297799f55a2d5f4bdacab1f5120a95fe7f1a1fce7c965ba6639ca8b5e570220539f941a801910b453db649c7abd3cdaeff6a58cfc9ca4504af39733d7760160ea15173be429d68fb8b2aad942f943bcca14d140666ef024eada83d73819e070c3da55a927c32bb8d48a6f70a150c492c79bb88b746d02e8b01aa3c7b0ac44903a999cb472185a884dbd3431c19ab59e0b1147c37712a5face90c5659db16f406d68dca1d65cdd1ce48a8181cf56c98c1330297c9c15f4f01e697d75c4990980b0ad31edf93a7bf63c2c71de6976433a63b1727f7b1101650e9e6d5a60b4697050ace5595289d049cce9e152bb8b287a624cb13613734d8422c9d8529485373095bf71c49858e9c12d9f03c9312f1dd4c7c04517f560838461ad0d801ef612804a8fb3274eaef2f7231da5aa283287c3255743f3ddea6ba367f9ea42b64d32804f44440317c784594df76f343179ec5aa18a7e63859e5f1613d3ddf0ebe9e14056221b6255c0e5e4971e62031e478a2c59487210c41c23f05b36ddc527b7e0a0c226fac36fed5422f37d1bf67f0ff3a65971210e6afabc39610cc5cd32c543506ec9a549c2b11c954a9d85cdd6612d106ed2baaf4b5fb0a69520db67edfd02d00063e688000c896fa2bc2ff314ee7104c8a9cb15a0471eb80a8d26a3e7a3c7200f0dbf5cef26cb062977aba85d0e0675bb66102b6ad59747582f09f4948ae9f029f7f587a58cf15936ee244a651586497b6814e40da9d3679171434838887450c7ab36fc7aad3502129321ce3e11286840ee34cc900ce131bdb2906350c41b503f6626075bae7b2269c36ea9cd56a32bdea572a821f1dfebb3a9680e8a3962d045be150295bbf979c97ac4d068c8b97f141d4275cec40e02f94b911605b23d708d328a9f4e0f73d40b4a8bbc15f96b4e556e2ed728cdf96fd4e007e2136b2c1026afe97e0edbc02c46cfc5da71def5ba02d688df388c18d59c358ff71dc71a605743bd4c2124bd20c3df5a6c52ab382ff7465865ee6f4c6ccf4f1defa4a91ae07272ccc9107e787c9c0b15137e86e7b07a9bffb9864cffe4902df06a4b16f9d06dac4276aa51484a372fb21825af98e6a0b99d271e4d69a1e66f24a2729f78d00760032ff12539f3c7910d53b9645a518a9bf5f2a91d87f3ca9a56accd1dba3085a515f633e07bdfe0d02ff20185ab21251ddbe5e79ce76c4f98d0566d5fefa0c96c464e856f7eb4ab76c8f9ec4befb6bb0390f580ae1310234d3a34394094907d9db6cba8b25c013f263ab361989af0f3d1d137e15f05632a86cdbfbf3a0b00894f4ded18da34f74138274bb88195291b1ecb68d458297a58a9c51a154229e070c14321a29aec38b8edb8e421104992eb977a0384301a7d5efccbeb0e4693008340a118715c7170e41af9e5eb3371260a126265e45898543deb240f9d627e502fe9e605a336846e787c6339992e67507a5b08434f9cca5b981fb65df1ee2450b81914beff9b3f150b7cc4c27e13031ad4a5dc54b2d9e53c7bedf2674cae36208d79c6b8ced670837b1030460843728763f2f9ac8d54510e5488e28551658ce0af2885b9658a6643cc62feaeee88b511b10b006693f62c52fde144d5e1877860dcc9f252fd6889db334e8ce17e096d1969185ece6ca76e3ac886a62533aa44506e4fa1b577a24ff7d1b38bd9d44fc7b9af48574347cc8cabbe79940a14885d5095ff3036742af41f51e9e161deff50134163180060b2d1eeed8cba05b9d9b17093e13ddc05f05a607e52b3c8cbf4bf2411f7d6bc8e098b3d5f1bfdf54c687ea0d4a3c076dc59242b4cdf06fdd59567f238049855296c8cc0aa3521bfa9e957c0f8f859e80dd289eda266e9e7d73c552e8dde51ff554dd242cc06867e2579401072530ad38ca0ed9f96c8e41e5f59a5625f0a3e0506a2cb8ed721a598121712008e29d623f8710d042fae53b8d346ed72068471ee126d7923d0fa10eaa296488022770e601386554c49127ba6b57bb84b315c98ef545a415b5ba9762b427c0dc029cb4d09f1879be0088d3dd9ed4965a115b6eb08844a6df46d8b0219bf43d3803cfe10c81f115c85b3264267badfa16251eba0d2eadfac31304c632a236e372050a464bbc66c7bf7788e76b897a00b7fd946ee7ef09448bc6b2c9749d794fbc0fbb9bcb15818e285e02f92f483be0d26a2e1ba8182e8a99483918a25664687e03ac6e03766c3c21a7963dbbba85c79875b804d67253672a3eded6f4df6f9fa1068be28a25e4c80463a9d09bfea4f64ce18db806ff7c8e6d5b3d5d8aa2a246ff00645a2929ddf2a2cacfc634c89002aec8884354b3acffbaab277c442e813e4307dc5835e0a5b168e489bc5bf427a04b46162d929c4d0ef4b67069c1066839680c588232a39fd25979ec762f4430815945cb4d683499ff58c053b6122916be9b0f57cc75b6a03bd9a1f0d06125bb14990f4fa13dbb1e2baaf8888e3f3d8ccf160c215cd77bcc67f3cb5c73d108c3302b209dd3f05ab7bf04d08bcac79873a65d0ca549038aa918ed0e0f25ecaf7b4fad62dce05ed2b4bf71198a51418b171d3e0e1c9f981fcef348c27c347c398c1f6254c04fa9f277cac279c8ba061dc3745d0a23f2345c7a7cb0dc1403e36b6fc0fb3e5aab48eb9096cc812fc741ba8c72e806adc513e637900a7f4700e12ae68ff9ad9219e9e61dec03dd59cca1aa9dc20d03063258c687310e062796d4b1cc4f30184c2aac5f16e6ffd75e8a107e1c955c0b701c4318f05093af1539fb4a136dd9e9b2130f88e387229f600a35348c49b201a10fdd3dbd607c1b9f1d7c7a533338d676442997dc7360afd45a9e20d3ff7c0c57a3917ecc639d7b36a3ca67f4b4c28ea3aee551031e5613c197bd4e70e5cb0918056a8bd806209fb85c902d0c5adcd97779fd235b62594073e665b2777c700def2bb535f53e0e73aed336721f24b26a38c48cef923fd5429672136d5ef84c06a01b49c0b85d3ad7f2a68c5926043919a5c957cf4651a4af44ec0e6aab651e0d4f1065d4f687374500afbe5fde61ebd05c2a4ce72f9d9f7d32a8adc526f56e060384b0de07d78e508579c01de519bc35740776dfee40fecb636e476795707207fc4759f77db0f606c9ef87ae45f37ddca97435eeaeba0f4061fca14d59fad300b5adb3a2124df07389f8fb6540c450e0c08d94c74b229b92679c013e2379c809e43f3cab23b3584334a7e4fcb9fd9b2e482467c88fd5a874e5b7aad012e1050b26a0b3360690db0013baf0656bada54e7e88882470beb2c4f1de41eb56450c02108fa049b7ae23dce400d8f4cfaa782bddad93b186edb9bf44011dc6ab3bcd0ace1ee260cd29c0cbe5048c9a925dd5f250dd3e216fbd848037792c87d5eb960d5c4942d3c4988f9fb709bb2de0ce32884b0464ffa9f84811a90d638cc42adf0889c69815247600ae5be3016f921b0bc0b4da259c9d8e9e2f84671ab3e5c1750295a394376b59a4741ca2572050221e82446972d4b5474e40339c6609505ddd09a86c076e501a9859a1caaea793a818e34c7ef1ff2d8bfd5d63cc471e8af8150f12336b71229b5c957e8332e27f64e130f331f02b9a496344d027110da2c36d0504ea1942a142a96a05b84c1ca7c4745036e6669e234b82d4b752cc725117a80168f734a4ca02eaf0d15b05597b6da69b2164bdc2c8d5b7ff60c101108e7184073c450050945d1254502ed61e6b7228577030e9fa562675ea31f7f55045127d00b2a82510f43db0e15ee1e456e79067af0a70e49103b6b025b36b0aa2da979502df5190684108a9dbc4350dafc6f053552a153d8da8f77b4bc9d6a0c8db21620e19822e06daf2275ec08e298036ddf5e3de32bfd22d78a12bfcce78ab2175f90d83d88d5d09a94b91838ee3aa3a0104bea75e585334786c93a042e04afd5c9a0212a45da71238d970dbc1d5c3e8b9d62705dede286a10cd48f874c9e4febb8d0cb7223351d6d9f43f3e019990a9a71ccdbd2287a7a9b202bf1cfcb890048a9c0ae29f006d42a139fe09e809091f7be7f60fd2daf9dc3eb90079f798821ab3ae05b81b68d21f8595527095e33abe4c057b6dc55db9806f1ad6fdb97cc1863f7d0c5b28e4f6328e5086072d81ef63eab2afe03fe4cb0e2eddf0f5d577afa8d11c04db6afeb20ede511606fe836629dcc1644d33c70802b9a9c86af5b7e90f2bf0035d3e2143808098342eea4f64aa2d8a30730e365dba0d2b2f7f7f653c01db4900d6fc98036c4a7016825b7a7c6d4ee9f50d0b8e2ac1ec30d3627275aa0b7a2a07dda9a1fa969fdc9201477d6615c7bec09bdb08cf0be08d52e1d0373bc9930b0d4355ca99d52a21d3e7ce0381ce3dbe4f06f51b44f671c984e7908cf90445b10bcf9aa26156177343243360aed7db2c750bbdce5819406dcdb65ff054c22a8e0db73854f8353aede1cb512ae0fdec802544a72b0ba46eb32f540ab876c218c703036884f4265b30463668060abfba02fbf927cf531eb2d2614ddcff1e6bc6b00d24d51316556ce83e4abd14a97a708442e90ed7520cbfd0dca3cdc1cdbd16db029d7199182a9ef3bd6a9ce3207fce1f6e6f2b456b2282650d0f68a9360626070db29315988acc4bafc3b1df927f70142a03a7efd77f3a3bffe77d07dab023e7081fc373fa33a06b6cc817e7cf81edc248f15db3eda23c53535b995d71b0e7c10736298e3bea84866e4dccbee63a45ae739cd5d125e81b66ff802cd6568038a6085752cb40f26d491f99a8f8244c6b15278a8516a5fdab826de6bc167a64532d0fff756b4f70685f7688cddaabe08fd9e31897457099dcec1138d0fa4f5798fd09fa049fa22da797dd58df838f4c7f90f050a04a19ff7bf184f305623f42cc8b0c81f8733e03217dd2c04763d67ec971700c35da1dcfce8e7de2570f8e0377010defc351d4a4b086acfa54dd89667b90ef43d8a076fb44d5e7912b24172be82704d53ad34986d7e4bfc4b7d6d2aa8b4994d7a7a161c27ff9ba5489d274dabb590199461d381f535bf9fd2a6ccb3edfb1e6f328294b78073145837cb3b1909511087aa64723f2603c65e3803105c2c42ddd76abf1a03d8dc78c202c629ec5e3950dc0f7fb18e1e17d2277752dad40759e1fcf03e147c3cd34f290ed73142fd3380e1f5d03cedaa221f4c392fc8960f03e10a5ca1ed63224c62b44064f2d65608a02db76a33db3e03a82ef61a003b8754c2f17e7d90b1279438230d1fc808665f907feb9556ff7ecc87d7b8317a192dfef7e1888e5b61af0365ed80e7262037caf069ec4100a2699245978aa091bbda4e623eb7a723142477d19fef5adea8dc59c03404ca65b63f6a9b85c3936a6b4c1f0f26e653f874df560859e808917ca80350b8c4a47846bd23a22791ba1619d752f2cd80a325ac05aaa9671c92782646b1803e59beac8a49680106d4ef164e62a7f67f2f9c92a88d866658e7808c4c030640556c269791b59a5e12c65a92a22ae20f70ddcb5f3bd58080e3bc9c010a89504070e3afbae4866d6fb23bc03efe21f68f3fc6dc6ad605bb63e540754c5597a35058528b83d1503277a480a76ca393319d2efb47fad69f2535a69225fb79084fb0dda7ede471b49ac39b5e8d12f7f259aa7dbdf0f9a5caa9a86f51852c6721ff70b74f4ee391e5c4245837e6f717f87265f8669fc0ce8cbc8680d9230d59a05c00593f1e7d2e31680ecb628aa53d0a84ac55be44a740830abc05b92aceb81afdd0aaf0218e4f08abb2344edd62afcb65732df08651fb5e72431ca0d2636e6fcc8096c61763eeb33da6502517d6c676404fa7f949dc3d3b29eb43f50294f3351a10a6cc40ecc0a5a1d2794e82bd1088bc317f2954f4290b4bd9f31bd0581794a5a0ec9ebdf9b58c4eb750e4bcd6858fb5b41dbd0273877bd69d081d3d8044c76a7075f9f0f07dd524691d714d719c45bbe88dbfd82b4a369c8844fd58041db6ce0079e2e15027ea7dcc06fe8fa3deb2532f072d34cdaddc196f547e89e77c7d35e0e524f5b62601de73a90ea8f7f4f0495642b4a27689d8692be7b1d123bab93ca0cbe1a2af759fdff624c921429198687ab04629309737f724a4d88c49a2ead9f093436c278bc79ec39d9c7eccb7b69499460e1c66af73cc48ca7c6c3c20abb24060cf6211a9f2ef7301316837590915bdaacdef25cf6f0034cdb95cefcc971d20bd2efd81bd5924ac37440c29a8f6e3bf76ba1d25ca54db32585040a5b9ef7340fde50d0a2d5cfcf93ec6e60a6ee4b0a716e17e1515822b6127585583d1b68e90a0f40e27038b7bbd3308dc8d87e394a47818181a29e1fb790a7e9818c9110c208d56d3d90692ffb3d5a21012eee6e6747f43ec10ed4b3e8c5eaa6c458f2c4ce093abf6aa3015ee2160bb2045c62112472666fcff014d1f92a7f1c46c54c36b60b6916dd834b4b57208dc15b25cba221b9b4e903f2b5bc3bc1d4f6971453b88607da313f35ec3e6b9a4a04be9b4aae18bda7c8150e5322f92e9d5e650a3de900000174abbdfad5c7636a77f6660cfebef3dca37033ef03250563f0853a538518095a58efca1f34ce71eeade8045ed3af58e5d19117fe89acc6cd6d31b392fb5b0642c906684f7e4b04f0e9a65e68d8d3e8008fbb0e999aef2700d78d20e5d1ce07fda83d16537cd48e66c5588411d88887db043c3b09ba16b5ba64146654ca950ac40be2d6995a4e64c3091e86f3cae57e66fab3dc500ff723b9aa29cef8ac680ad3a621f2305736204c6dfdc7ae504ddf838e2520f2b963e04232fe03e0cc840c8c9b4289d5db7ec757e5c9ce3886f438973a4002d7185fe51f4616db35c2c00ac9c52bf9d74dbd9db75ac18d498a3daadcb515d7d2bc00ab973417850ecb340ab2cbef9c3e6958e25e6870b6e27a754e824499cccba9089d27904032b0a6f7059d7e59f5d4d731a064c08558f3eda76d95d366d91afa54b67071f9cc77b8b502e302886f00b07dc963c83f70105dfc0361df899cd00cdff0fc14a87b3925210a028c898faf276e741cd1ed71bc308d169fe60ac9ea8b100a6283090395f51c0900dd2b0393507f14dc232cbc0a779cce5da2bf7496b2eb86dc5f8e5397b18002aec4b3f68748d9a8d4d30a676a4c2683d5e27edc57defb7d225e9baf5d0e9307161af8c8476271c6e8ef3e18f89c4ecf46662cec614fbc3f6ec2300a14c36201579358db2059fa731513ed0512ce1088e4970397489fc117c2999b136642e504f8b7fefe2ebaeceb67c2d9c3f9f62afb95def3b994ee2ea53a50a731bf82b109b198db7ef55099e6c1dd1db5f40bf7bc6f12f7095ccbbc31e524339b3e91f60add35c6366837ee39c29b451a557a02025f219554499330c3199fa6e8983d240bda14ee3119e5c6cc62abd4e699365b868cf9275028e78d692c365afb1f19980e5b11eadb5b16fed145cee1566a948fdee38bf5eea40f9bd7b81197fbefb2730d27132829847e5cd30d7653a919c6f65ad209cae7acc818b0f773ce52d959a30805069a2d40b5a77cf0d61d7df641ab1e7b2daf9e5819a0eafa88d1f962c7f90befabc29d26a79c34e6c5a224bd7ac1d49fbdf09665103e6f83aa3c679b50fe02b0b025d23944311d675fefba792c6164588baf09346f2aa065419a0d4d98a60ff2dd510969ca4587c462444c2375f49e1ec97631f9a91b2f2b6f012fefd073045b9d9ca9bde1d864ec907173d0ac8482f5526ff65d31b74f6d5a74e1e1b7f008be418080dc45e8a219a0f18317521d086e3b97e3d5d9a57dcdd59156667b7b0e43c6dfb6d2be789a62aa5f746ee0b9c5da84be06cda7733563ac5b86c024030ed6856456aaddb1107ab757971516974cc14f48f5438c64ad139c79debd4fc40d728e935dc6cdf2d26d6cb6be0ae6b8336c27b176ce2ee7b86b3157db094a22048273018dd361a570911755934960123d5d2b0698c6b497f170332acc020db70f7af1f7717ccd147dde29fca136b8e54397a62f993079e9d819e9e3a80bc5c10a7952a567db0a747d5b9159e6d4863cd0b4d5f5271b11777d2233d26e13203a0585b2fc2bed0cb652aae9533166c8d8e8c7fdda2ef2ab68711042fda90886ff0f49f65df54341b78129f91ff32e630e97ce8c360b9d44e0513b54a9f77ab071034a74543b403911d554ced7cb17771b2137bd353080beefe49f226386b33d1b0b497db1c4e3389c184cb30e5a82759d25e9e0c658b8fb3864de0a1ab9bc69d60bee67f52b109b8136059831fa1692f6bdb1d3f63bbdc0f386c9973c6ab678000d463b6f3c03b40e5546f41278a4a2175bcc03c888937cbc143f047985942780038d1fc59110e85aa4f40cf9ebf39e94cb709fdf7653e5981e0572660f6c12f90810a74adf4fd535979b557e6a1d38d1861d58fe36c4fcf3912ca8241f4828d505f925b4ed59a06b456e932622eb2b202cf8a29c9b31e4f82aa74b1e8f4d2e0606ba8ae6920ffde238075d4d4f49ebb244a17cfa854b30169bfe1741f8368de7031720f33d1edc6feb28129b0525edf7098bfbaa7f0ab0dd7585b6e9796a253505a8cb055a15b6c708f1518788dd611713c25b75a9780075db9ba6559fc6f20308bf98d2207ad42f9c8aaf3949f653cd0b3036350ae42bec75610b37d850bce105f4609d6b05d5903a207a673f6f8823cb7a584113e527481bca3e3b4089dbee093a35befc3df987cfa3a293319516d41b09e96421360da05bd6622ca4da15e30e4db9395f4fd1ad6ce4feb1ee3c2d046843c76ba6a067321e6b8fe18bd0f7ff0bbbb246deb0c7032755c5634aafd34fb44be0a2923a83eb6bac7572640e4a7407c35392998badb86c090b965469f13790088f7dddc62a99af9c71607d1c33040d4e4478b62614162949d8dabfd7330534775e206cd6cc52e848421ee4be054e0275f10e6c15ab9e978454071c10a9f5483bfe253cf29a661cb028c0c870f2fe0d8fa4c516b1962ac2ddd28ad190938ad1a79a4c0ce73d7d2211105349ce79bb0d32cbd3426e8d271caf7e8e490ba7106156ac4b456e54c06596fd733ae8c4e30a301a8724f2bcef62cbfc40e4379f19602866d68e8872e5b153f3e348dc2c630c6044690dfab070f0079a46e7ee4146996581c5385e329a2a6aeb6bbe3aeae60cd0d55ac90f6f3e14e6760b65067694e18325df78a0b36e8d2153c6d67dd9710a46756d7197173cf29c9ca7cef95d3c3da5ff446083019819e22be8af3710f506c8bbd559a1c1b7bf971cf6222a7bafe7c2f24e51afa096dfb3b224899ea49b0b8eae694af0824582c1818e7a8bfe0ee88061af5df9450395f2fee276d878b60f04b6698a7e9f1ebbd8ff509cbe08f8e1d3c64cbcfb203c708cb127c4691bb202298e4f8e87d56e7b6b874bbcf91d94276f8cabef4a2a5ab6c636faa9d21c2406473091af2f62ee11ff131e2f89ae57fb6c3e63d0eadf5648c940ffe7ce898200f62b839006a101c9aed0ac17a01822d4322bb47ef67255fd21136acd475f0c0a201b8d30e878bfd605c17a4b219b08b460c3df5e57f5c20d1f077609f1caff0260bb99627cbc6352801964659e19b83663407d2c4c5b700ea7f23189c5798b08625d56d9bf5084a2267f00c333d12f9fb51cecbeaca548e16f69d454da3145028f9fe17dd2d592f8e2e7821ab8bb9fe03e92e2ff6cf93bc486ac15aacd00a70d38b3b7d01660157136060903aa065e60bc95b7be9aa8bffb50182a85cfffab0fd4e20fd8a872882e70e8c8fe4d0724c52e4a68283864e85bec1c0955a31f3006da9b0f5e611ddcbf50e10752cf50da1ec624bfb43e731dd76b934f675a25dc05e494edf77ef782339f2863cc4c894a37d7fb2ef3559909889e1a4bc78f876c01e99f24eadaa885b1540815b9515b4440cafb98569e204d629b1d263765859c069aaf1a846df429f955609e52db95bf9c51362831f1bd1141bac10de1dcaaf00504956efac40cdd29945761c86ba302dfd47ace914b3366d10e03c9ec29fd3c024db3272d6d5cf99b1936086c37443afc2e82f4b22ea558930f4bce1a8b7b990b6d1b6cf26a24c4da5e0d5e323ea45ed01420b6ea080c950953c6477ee1fd1008e5aeba175fd3ef6f83e430809764e436e2c1ed0713c002592ebb5266257cac02cb1a0634a43ef92e1d55845bd797ab236bc4c6f5baa6781287335d2b98c0200ed685ce8741d804df30136024d1deddd052fd753a146d33acea68f5f3cd03c4090a9e520d24f881e65e17f2d78b425ac7815a9d5dc0dc7cba90b9d74cb30fcb0a58fadcb6cb3d27196605d3b570ac7bc324841d5007b0af6b634be9faa1c9c50798fb25065a91c5be8a483d18275e3805dd9f2d4b9b5a3f760ec0ceeb3c81f5043034923bf9ff716e5e7c31d50d71bf3d0919d1cdbcc22d4f333116e1150fc3083a20cd7489322b14d99f5e75db6b326ce85bff80cae291eca022f51fe543f706fa37482fc01363077e7e29c5d4773002a264692ff93662da043aec25f5f53f0880093e2075c865071a8f43595672dcb7088c082790ca7f48d5f25fcac0d3710c9dfa09b868e95d8cc7eb0eb0f00f18b9f060d2854f812c5f074a9ff4a090c209fde2403fedaa217dd8cca7d92cee21ef9d4ca4ff8347fa9eb64cd89c4b625105282e0bce3e7a8d1b0277cab96042400a7955ecc4be5b1edd4730001c3e3cf80d1ae5849b1896889be4cdb5391060c3f6a69254ce5285ffa3988c0643c1818d019674694c8d10c566964e66c917f98bf049acddddfdbc4675bea4eea7173ae9012a030d3c5eac7622d5d37cf5ed876f662bdb3e647d8bf01658b23834f950b90d57041a7b0d28d00ab0ea140cd73d2177ff2f9098b815bbb88d2b0e8448b8250a95400abbaed03c7ca73aa1560c282812dab17d0b0e6aa405d22c80fc8e03c00ea54156fdbf8a91a10b29d9c039cdd0bdd3271bb1749a4307bf1c66602d29df0fa633166a79b894123563cedd6787781488064a58095eb05764c3332ec0a901054b01a48ee4d1c5f2b4054c066aa25fb748a1f10ea08905db669011d9500466092ca37d1c912db079453c5a42ff2f51a7c47e78935f9fda33f3fa24197b23050e6f452d2fe416b744d45e10d9b609e24b7a6d4a60ba0ee7af628b26b2951b4606247cc9810d70f21040f9fccbf6830c406c00df43deb30dd13aad2b6ef380380ddf76b1babb4837dbec047b8b484b40070fb61693b5c65fbd40f7d1f85a780a099f342b33a75c0ee76f854a5a7e79199363296fe2f40c5e3daf746040e21cf5095d81b02e0908f40fe6b1aafba47ec0a4ca1ae9036fbcb2583e683437c50a5c01180514ec08e42d308c5853f4d932d7d45cb78fbf145c7ec5e16c980ba7ef790ce52e9952369231e8e9179e4bba38f9bb68f8c11549509b19f4988938059d880d85b889d0ec01439ef77f021a676025457c69b107294a84c42fd5557e5082c200b5326196f0165f9292246e02a64a833cb360d29b8bec4183b62761c0e68f61007e9c3153768cad8b9000b79b18bc4a8d575913c5b4f18bd0abcc8c60dd181e076c4dcb9f917e7865b09afe8405aff0f02cfb529c66486480d85373c193a72c00b00c279945d5eb7b425e0f89862bbd445be90a798e6249d3b29d8ae39642c90fc2cfa3ed320ad4dfb41979450a1f268cfb4a33342265bae8d588cd5c6e6f0707725eb8d268745def25a2ec708604d635026f983a534260f44ab6912cdae5bb0dd3f8015f3b42fe2073151276c3f691b0988fb8c15b34c686138821ade790790bf2a712ecfd95ad159a3b03c91f44a6ee5dfa623a645e9f8589d27e073c295d01cae297f3066062e56ec01f4a1224285014d8e1bc0e7da54f77d973ecc7fc540ad702b918872fbd9ebf3270e4ffe8726bb53fefb80fec229e21d33b4cd1ac3a01511960fc4ac2b245583f6a28f587c01fca909f98bef69a25e4a7745a31613b0995d266594c3881436ca5670251cc1d3d854ec16a7bd0caa9cfa83664a64ca709049ccdee996f097eed42d1f07024d71d1b60a38792d9bf437eaf1de54fa4d909176c9eddc5f4e4e552c45a385a766910f49e93536c11b5b5fe6ca3740867c70f3ec47630936da4b882b5e6de431a3fcac405c44cf6885650e3273dc4f45ab2090dccdd25dc0b7d4f91ea179561f2f089525be32c6e1cc217b195934f78468f01a5475324770f4650fcddac97468653ad5234c8fb7a862a929db948b180f912074e0f68ebdd920301b8f2018e281986f6c7defbb1b844690d1ee56a69f5b26c064979e185ee657db7e5d006af484a007c2674ff4d054a02a4503dc282b7d2d30b91fb77457c9d662ea7a2dae353726b80bb8a281ad0d1b471987cf6dc49b31105c7cd6021dee3ee88a346deb690c25ebaabd3dabe4a83f41ecad8743ac0bc100b80a6222c1ac852abbe16b936b480cfedef57e047f4ec40b602fd9aa8f8915602ffc7390ce3d64bc252aab89ca0c6cb1c2f1e7deb68e89732d0b943835df1cb00622d29d5d6b2c2c0ae5637bfaf439717c205f869763fe08a7c9a375b12316f0bb0c3fbfad0699981a76cdb8786f497e6731a8182542b04cf52975766eeeb0b04849bbbb58c8959c2310d56f64da5243bc7e69d3509cc43d5712321a24f0b800b6a7fbe463e82764655d4995058d1fee2807ca3ac2618604347ee256533f2d70eea2779c4d7b16ef792a9b51b51f41aa9224d666f9518fcc8a533ae9d683a3704964efda1953b1a2501f392ae159c37bb95fd963e7cccb5d4e89986a54f9ca00b0cdf7ed2cad879364eb6a492c925c4e8cd7e9febcc3d70127e76808e2034d2089e089746539528e9d462b1b69c3eb1cc662576ea62401514e220e4e52a02dc0fdd9bfb16e7a0c15f9da2a1a7268382cef072fc5a2beeb76d4990981414e59508403c4df19f5eea4a3e5bfb930a12dbd0ed7ce340295be4a74f84879a0a63370bc468ed08933c5434003992d28955a8d5d428362be2b26e94019d4523022d7206b941f49dc5b26cd53e459a6f0f360160667edce12e3384ae14c1896594de090cb14f7b6bbc4774fca5b6a41c794f7f630cf6c919fe426df0c2ea2f4d437a2a08580f5454e53480636ac1ec3e5828120538f0a59f80ee669b7d269219914a24085f842ce4183448c5b57ae50b78a89ad8953160f4525b33cb3a95a00a0ae5560e563308dc3d98f8de85d25d327e796ce993a1d472f62f6c73176412b08ae16601a347b1e0660328df0d292ff22f001694a05ed102afeb375548c208f51d2d350c865c7d6cce4ae069abbddba47e1d828efe58bdf466930988584f83c8e5403b098e44a9e36a3a5ec0ee697e28d6f80fe5335014153d39264484bdb956a9c27c09964e52265bb0c649d88233096d1231f1ce6596e6f7b9579a10fd1f72fa9a720123430710c38201d27b8754c13e20b8e1d474f6d16767dae089e55afeebca510e4c5cdff2596c6841bf24900c506bf671311cae56f5d58997dae38b17ee5e88030fca31991449ba35011d485d69ba4705e9455610d26d5232006309a8c603ac0bb0175026f5c103a4584b8c89ab5cf41cdd91448fc42cbce564f6ad44372bd30df466accc806db4bcd43a2c228365d45427566529fab4ca105907a52fa950fa04c69c793e18f93b973676b8bffbf98675768dcae386ff723393a100d294a9ab0781007af656b043f4ffa49b5d65bc0a8ac636e306233a80ce10e7c28fe0ea7002cef87e76ec691ed8a3ce225ac392e91118819f2bb9008c560123699f8949b409d7a6616dd4cb18acf741899155fb48f8a3a89475ce214fb92af345a5a672bd018758d81e12cafd76400a04c859c74e631003fcab54be165bd6b0fba321b9fc09f31a42d8ab7356ed749c677ac765169badbc4016d0d7422303c9b9454a850b044cb4e2863cb93336bd459ca87aeb409f893cc7c99a9cd0e1a879ed71ec393606b07a93ca72711c79b732f40e0f7a07110f8bd6996933e48c3e708ad1deaacc0e73e71abd6baeb56fab6db3d0fbe58841befe9524a21dfbd962b33d19cd667b0da2152f4776ab789d4d8b7355590e0e1d8747ea7731b754ec96b5a302cc329208e9a728b795cfebb7885b058dfd8776b1bc03a6d4410959127ec0b9b78e1fd40f8a88f00013af597875235396851185f3a9b8a3efbcceebda34d8884bdbc4630981f0b78843d0efdada7bf7319ea4832d9acc8714929478122301dc95172db20c5ce197ed17748f96c9ac4b71af3abe9e45c354017da6788af6f433838153770215bf6b689f622e2580908e67ec809196399adcbc1bd71e7fee858800bf4a7504e318573abd515cc95a81d5ccdd9a730623668f120a40d7600275dde3199c880b8a717e324f88e33d485a536443e523d3d03b6a786382c0600114bab9e9ef910e3781dc8558d2e58a6ee715d6446da18dffd36d232b2c4466aef326b72e0acf01b16bc5a8499f741627b67508d1447e442978f8e4aa753b3b81312e554060b209f78ee9ddd26477e12852b899ef45f95ed42a797ae25320b302fd1f8539350202d75dee173e36b370b55c6ffe1e9ae2df9b911c272a7c1d7a6053f01d2c67b903b7e9373f3aff710b423b1dc3bcaecb578356022dfa274d438ab42f60fb782702581a12b477365e45e408cff83f9404c2c96782998a56b7db570d181b6c21ed01d50ba865b69b74d6b339d0e9a66acd10cdcd021fdf7b3d5c2e26bd089b22940f5222cda8cb6ce0670dd5195b2c787672ad16209357ce4a4806444eb2a9104506516270541d6d5c8b1ebff0aa1c46b901f83da6222ac5463938a55bf043d3810b2baad7b4472dfe71eb65f498d56edcb2dbc8170197eb93e3564302623f6dd90d70c78647415cc84d3f09011e1ab1fc7bf9cddcaccd730c55ea85a9b23b4d1d0a60a395406b6390320fe8589b3fd00ec834facbff63acca1432d537c06389e60ddf315a1088c428bd367a73148c8f4f84970a04b8305e7112c7332ec15056d901578e3e69ae7aedb83332f8e6c1ff6735e61beb3104768b923b50d1dc687cd10365e9c74cc0efe5cfb450d107717433b1913ff2f74d2770364eaee0d96a19530888bd7be10f0c2a1100c5449691d7e6249cbce334da957370e3ab440b091d3302da4d43b90665c2efa3e9cb8dd43df998dc88b71a4fa009fc47e8a9627aaffa07e8b7502d48ae503e34fd3ca72e1518b866bc8054d59ec6ea2abdd08473e6630f84b197fb4937d3f48fa9c94971ce57e915a7b4102ee43e366c3d2e1b8fe603024d697f7e1c485a8d95498e1fb9f867945814aecc76595a56383a6c93aa7c6e0d8fe5fcec7ce94c5c38e5a2214f6cfed2afb3bf098fa02c6c73ba888994bacb05b20e75b8cb715d91b2cf5da052ccb899d4edb848c4bb96733ebc6c44b204170b6d56542dcfad96feb69c9f67d8e1ebc404529a5192a88bbe10b65edc0bd3a804b181776a66f1f65cc5d7912db8562c3b71a7566e85155ee3400782adcc7f550143b4225ef4e6c39af102717676b77fa4200b137aa460ad2a1e207bc428bc870d9bcbd0f471306fd06570bed2b8c461786889e0f90c2d99ade0c7800eaff9280ea7ee7206cc701924a41936290aebe6df5a50f30cf7b38493750befc81c31eb05336a7d7fbebc4c93c041938b315eefe2b6d6f1bc3131f68e8c021955aa49b40c39d432d22177ea49cd35073d4d625b32367604873925530b4f560e6c68d8e40088c06c784e20341c234bb177a5e78884b49dbb32ae88a0e58a9118edbf44c30f7533a4ddeab25864c4cb598481b0ca2cb0df82cf2791708d0f1e8afa4c37590788e90f2134e864cb33c1cb33a679c37d54e47ce1946b95e8521f0bc06a0cc40b047aba0953552d8c46323761e3ae5364be507481b2461a0870d781db7ac4e2003c4300876ad038971e09d0e716bb37ba0985a16ff3a20bf4d94f6027c44dcc08f589451f831c02563dd56e93cbccc0fa72a5e51041a0d7fb29e5819b36e703061f82608265aac18b9e6fa04c1ce229f9e9db1b61d586ae982e2fa0a26ef4420de5ff831e591dc1da44b02ac4406eedc0462af0e631e58e13132f0df8bf898c0a9c8161c942c87da463a1d20dbea13d4e14cbd2d95301347cd1db71bb021f3703502a86e24cf2178aa96e74e272a077ff0ce417a9c33972a9a47f1a629e00e306f626fe42caa70c9ea2cbccb7b8233a03a969f591f6bc45c159444609b4390808716718b8bb5ba30b80bb64dac6c31154d2a2a1cea819330be39be4163801920f5034b934ccec4eccdb6c7f1cea00c113a8b90c0a9416eb3881432cf82cf7fb05bc95d25dad11a6ff00d04f8ee241aaf69e7bee76da9f00e910cdec3c61772002534d7f5b502e5c971b08ea46f8d2f63e57175e98da86237de538b1996a439e0457ff990e90ba9d9c452540dc9c4f56a861d8e4c483fa574f9cf8ce9a70ef230e29ea76ed383bb4b45571521172aa2a1d3dfeb48f64a11ea49fc6ef22d3aea70cd10f6d0ec6e8d5ec9c2a0dc104ae1e258ecdf239d89364cb2152d1fb64d1e105d47a7d585b58a595c3830a25589440c3699d4618109904161c954c0d25a70e0b13fb3d9e93f2f12abd1d4f59d09c29268c9f067ad706b496037e59c5d881520959c5d8d163464fc76b6448443a8465cf6394c23387670d06c3573b5da67e2e0deb401bda23b3945f39cc80895054882977c377fb747a3b6670951fba3b01c301b8ccbdf83d08708b83200bba486cfcbd0c39d957f4737680fcb1c9b219af4d020b086197ac87ffa2b1ae33facb31fb561700b30b1ddced0134f4c17fe64f6800d8b25dc85af446e56ba4cba9b4680ed9f877fc2246a0800becde4dd5991cef05ea219459de75754ecf75e2a2b94c747c5f45de8e5b2748dbb019558adb7bf506b67be758135ff73a082f5e9160899318d272ca5548d593d2c9e633eb2d47e9018f41e1b4102e1658c5fae14e1a39ce749d3d044bba4520f525a6dbef6b395203fb92601b545420e316583e812b40632f8e52bc0edabeee4adb0d75c8110c1a0370d2dae17bdd393af635a966c2417357015d4236793f1dd8165002bc90653802f5f5057a6d08425c04c677064bdef070a1e1876c9c989950edb3c91fecd1e20bfeece36be74e6155966c13293eaa344ef083f3e9c699f92d95eca0927b5b3e0425542ee81c180c6edddd2b8e4063fa6d7c843ea8cfe8cd66f252d07ad4c0e20ae858a95c169c9f9192c543667f3c11ee0a322edc4cba6847469e4a9c0756fc0f51a48bf76e6c59ac81268bdf0a3ca16e7875bb2393d42f504702101b6d3fb9009b7150582ba1283a2999095130a08186254efb57db34d8ed8f853b57e75a3a06f84d4c75128a7b4eed2a640d06246800728ce1edd0fb935a7143f3892089220e1633f5ad7ee028ac5c3e497891e5734c018e803ab22b79c8ec206aeaaa201d0c3c748c83d596bb0fd859f9bdefa901f5941d4934c119ff1fca315019788b480a112197330bf7c47502506a73b80d0cf1d6c6ce8c9e9db6459e4b0ecdbd0a270e28fb2f942982bf1fdcb9e4a10c869bbc0c958f230b25ce5c3591d99746060808ff286532df45de5ba3098e1f1e930eab90e4174645a0eb407ec901e700c3e105933e6e90f0cf75ff51e18e1b92e48f4a0d68cc7b8f30b56ec60bb97502f73a0c05794d3333e52724af0e8c82ad80fe2a883a156d741d0eacc329e0d0b932470a61726fb9eb2220f455f2ec0d1e8d280df71cf2a9fe5441c858d05e6c74d6130880198f14fc6de195ddaec0dfd6bca0b0b716ef0e5f8d68ffc04c9c522b35010ec4934972c821c288c232d389deff24ba1523bfd7f23a88663753a3cad7e3dc00a018dad7753e939e692fd9316485f22d35029cb07e05b10cebf20723722cab0e501be0ac5fcf51971c5de84c398ee8a7365c5f35e786ed3442ccce34b4a567074c4662c6fbfcbfde75eb4dd177693ec275b8895ba3d2d6dfac29b7e5ba20d804cea926189465ecba79d41f6eb3ee55f77327dc4414425436969cb6d9b752570a1eed1ece4fb56a35a0982f33ece162a32d5a83c420ecf95f14256eda73a45f0c1368b7ed9dd6f1df2cf28b3048488ade0d7957bc3c19ecc3bedbfe601ed5c20f6d6203599ba5dec674bb9c3346e9c76a064cf4de4ca01c3ea68d76c9b2589c09be80a3d96df4a34062c7630279cf942c13dba0a13a7ca15e746f3cc20ae2b10f0d6ea8fb1d4e44e22bf44571356e9589b4082c5e186b5c5d43db92292d854603aa094fe10d6eeea3a768f6670df891ab6b533214ce367e0767fa0d03fa1fc200366bc1f952dbb4faeca724e929d9453e1a8e73a2ed60c9b144e2a3780b072e0d619e662db7ddcd225b6ab5943d7f3a2ac02299489194feb7c3726a572d6f0803c8fef25e8aa1b45d2f67d2050d55e16ad07ebb26232a80ed56499b827aad6104ce9a23fbdbdfa84b85081085205b71970a6a6fa066a5a482162d96df4169ef01c59b2bd45741f528f7e91af72218b45ecca84afdad85d5b3d3ccb1fe99d96501f5a379e0898d58cb2b94b2b33becda4a85afef72099e791b2dd75e1bac0ada0cf7b579a6add93a91c21ad95b99fc89daca8bf3c77747c84d48e4fa702635ac0888a33b8ac0e27ce775bec0faba0f771d767e469bc8647ebf8937aa7db09fbf0247cdc3641dc9a8b08f6c757fe32a309d191b2767748b6a22f50608dcf421aa03c7f59e79cfcce764dfa07cb07e5ed58af95b4547515dc4861dc9606671b29c0507f291ba5e116e0d2d51ffcddb14aac9539c5a929a3ae1cbbee50797b481d00e1df43900c188a91b7cbcc5714a1920eff013184e5d3f998b80df683c78d33e0fdc3fa62f47740eeb99a6a4040dc4a43930150ee50add4174f5afc8a5f4659408a8b35aeee87f255bc098e89937b841136db2a33bf74869808d26e05b9825620f28414f983d64409059afaacf545c7d2e821208aff4edc487a9449d9f5b3aa90eb27b95b85e803241ac090212f302c7fed9315be78b08db33abad579e326c53099e5da1c82b20f6b476d6a39aa53bb89dd0a1fac22dcfcdaedecc05de99e7540e9102e8a288c6af9bb102a834033db51d7061d105c6dbccc467733c9737053c06d4f3a4323e805adc8624e948843f63ab736e07ec8c81c3d545d99dd66ba1a3066876e867c9d1e8c5b03bb7879e74589305cb7d4cfd5700b9e47a8bf4bf1a7001b68b8d72eed0fff8ffd5588ba3bb70f4bcfa481ea65bfaf5344a86de77fceb04e70caa12be765f60dd1fc240cf84c79355e7a11861021ef7f0534ee17f4904050aed4ed5ec3b86776b4f88e168072065463b11fa20aafa850dfa5e977afad1060fd05583404a42462cbf28a29527d84731ae8b988437297524c9d65f7e96e10adce4bb37013da1a55702f859aa2ede35dfa40819733b08e7b1bbfd71c55deb05a6e8e1b1c1a57190420efa1a5130e268b2f4217c966055cd3d24eab604cd92037e42ba4c5491b23fb0f2699f0fbfd72f96a1222eb1cca2ca0b086ecc78cd8a0733b4d41b52830ecb756fd3d0e062cc905438c78749c2b1a6a9b3aa10fd69cf0bd82c38461fc99d6c904c6bc172f3247106d5e020ee60c13649e31478ad06600660f07e742c58ec17d540d53b466ebd340e4294a385adcf23095c5917c5dafe04a213919c18af87278420b730658b32a2b5a06dbc59a7045ec754a0d90bedb30dc9427af43e58c7c2a9c596cf9a0b4dba60fc3d6dcac06aac96362c5f46bb73089ce261a1a10deaed768569ed770036ced8aaf1e6b5e65764d85f36a844c75f016c93e19b74db9725c63baa3f367002e4c9a70be4b89deb20c0c03e86c2c8470f40fc0158c0fb5866af83a1dca6353868479e81d01351166c842a0f384ba4a607aa61499a57506041b0e6782ae18eb5730487c2d0c81ba4ecfa41a463623cfb07a52548f5d641966983dfce7aa196660a54185e96a8570fc6d3edd8a69c7f490f7dfcdd9cfcf77c374df21f33bfbfa182e6d5c6235b7fd9aabfaebb15ad1eca09fb12f05b241387e8359c7dce0cd529b3384f84802cb5dd37954323ca4d29000afa92c3c0947c6290132e7033227af6ed3713a5b2b10cbb3686c84b596b9cb7038945a8374b29a0dc810c80e6a891460ced71289d52b64bcfe32e132494148c0a71bc46dfff523a03d78b7ff719442097f641b63188296993826d09f0a2fba0057e40a872be4cf4a315d5c10776b2d429679e2c31443fe895cc1c45cb3719e2018b46e4461a4e875ce6c223e0b1a0f2d38d31efbbf94ebb0f0d9176d946bf3d09d4b2c142e266ac8e08f5aa2540eb1b91a5ea398b16b4ad40275156d0ff488e0446a477baf9cf844f9945d74bd8b56254372fe0f04f263f329c4a8797c187d501ab0182ebea7ad269b371ec5756a3bcf3063ab205ab8bb9aa56658736065e690d2e36baea772881b45675ea3835992915091079076032d1a11f03ee58b8ca6902d9df91d252bd040ad417f97640458b0faad830391c68f96a80b9d1a183b24d00ebdc73f0f8e2fba2918f91efa945228c97cd7a1ce602086dbeea63477b9c940c1dc4e03d833d30d6e93e59c73bbf8ab23435a207ceb2207ea30bf36919541804d38ca1bea18ae64bd9afb5e3f05bb21c411249a73ab0f273ea4298b97243440946361d30b05b20b1347c424bec48da7e648de5c4cab89728330ef3d6a3d8a9085e305edf3d518fba68a988d4656981c06b4e5eebbd2c8b443ac5cc4d51ee170b7c887adda817e9a84b7d3e11c87e5ac7570e1270b80299628e82c1be249e22085680f807bb3d9fbea52e632c45d50c94f5a00f3e36f579af9f50916e6346aa0b18b32aca8846e4f49f8134eb03c7c3c9583f31908faab26af3bafe77fb2cdc04a6d74dfdd0c50c9044cc5d9fcbfab3a72d8fd1924746b18095c632dd914d390daf13e194fdc5bfeaad4ee0e79ad495833ef22e47ac3d367facf26cec7199ad027220e6d3e350c849381bbbd82fa758a4f5deef8a2366a1fc8c81ccd7bafcc60651557392d59a606fd9327178adc26622984c8ed0f11e251442875a3e743f760c085c02ee85709d8c6a0941eb0b957174d1db3f7f663755357d178d524eee260f8145f854e9f9009dc16d960a6e0820fcc78e16a0c0075ab4680ae4a842a5d004978048b72c25ce0e480435e4aa427742eaa049ca3853923349b9dc86f64e4c0fe00442003e2e0e29e0275a0f34ff2b683911a04b1ba07a9822260ab26898b4085ee1998680eb36dad85cbbceb7dc5b8660306ff4dd79fc6d03d0cffd0ba50f032d36f9023016d7ff92835e9367915b555b750503f4fb3de1e00c402ac81e4b0d6aead6bb9030738685b0716831c85cce7855284a0e3f8d7898c0e78aec9add923874d16dfcc50cfc128cb1c37563e76c4a566c4f7ba8bc6cc604afeb6883331eda505c303d373a7789aad351923a8ad4a98223c9d96d30d3512bada90a82ca98d9c89a4bb2ca9d67a7924a14015a06af049359fcc9f29b66ac7eec3bd2a824786bdbba6a26afbbd983cedbd5dd9691d8081ad201651f27ca735d974a77b723f24a7f1403b41bd3e6a784f4041b3a5f21af2e7b247718e84ec4777acf7ac0d22506733ba42b83186e96121cc0c34737b712f7d2ef586155d2fbbb9ac0d9d7ffd758b32875d70a205da43fd200459efd6e2fd487faea0a0352639d97f13c8758aac35932621b1c2be82829737819a2d9b107452d23c240e07152b2566ca73b63f172e860f35ef20a02c0fc4921d56a747a09de982e33483c071e5253b5096fb6e4d6b57ccea38f055b9d157813c05d5588dbf0111223c600aca2bb62a8b89aecec70ad52010ec0e13631ad6a4cb02d8cba9e53d02388c11ddced0ea0a873a281e2a1d3df5bc862141b54237e77fee21b302c39749cace3b7844eb0a6475b2e1b694c7acf2815347e338b2e753bece5cb19a0bb46d73dcac46be59d54fc15216123ce3ceaa6fee8f075535d117d115b35928ef9376a6d0780ba230af3f2ee92a0749ef58b5dd33dbda2da269858eca684cb727651116a07a5e5e3161fef275dfae1e33125cfb9b807f08c6ba5d289f9af95d53e46ab47abbf003d92dfa4790b932bcd9f22a7d33ae37aa97414f295e236c496194c249aab3f17c7e35cc432763a93816416ec3062f95b099bd43982e3064c55fd9c5a27a6239ce91de23811c4a67e216784dde33e79eaf079716e220c924239756770eed3c46a1df39d8052e9a6d124bb259cd16b16fe501a3930e3b1dc85264350d749e1634f4ebb991a565cb2017509809ce16e6cb3801e11f826bade0d3df93743a668181f4406cdb654d113cb784b389a283f5dc13d45a97882a183bac386fccf3925b233404fc44f370f80dceb6056572f0d3c5e02feceaeeb2135dcefa05d0f6b6551e869e9cd6dd403ce8ce0d35534464229591b6ae26525bec199d9a252f53eeb2cd7d7af32d7d7da8ce27530e7754926a92457994710aaa754d39f02fef5354e74d8130c72eaaa59238ce97ab3d9e80d8d1b8141cebf6d76839fd2edba68ced1c4d9b6a4f6899225a057911ce9066916f8d7abd1c41b0af9c13ecd906593720a63849106674011844b08cf6d4bd5a0fa4681202d26b154e3bccb55652efe10df4493c005959d81b60a95ef4ceef474fbd0eb61ddab134349ee80341f68c2eae8eeacbd3678831175ffdcee086de498cd97555498ed8ab82e76bd743f77438e8c8469c7ed5b61a208699d00ca8b1c2f199784113b3a1dab968fe6696bd7757f6eaabff3be7cb98ce58634198bfc368305934edcda5b1dfeca6aced6974b59186a288c012281c3424fa76aeb4cda4396bde2d5cf033f130d7683083d5daf9e6b3bdf7760834a435d79858ff8840b977f7bfdd9225a5b3d8967e7713a8148122496914d003b5f449a7a3d4d305fed7f2d5fed7a4bfaadf6e4da097c401d1e4da787b81c6022a7e29cbf7b0b9a5e264e4aa5f45da74940471ac940763c7c7ed8fa2c326d0f99f3f8215a2bf8e617b05d5513cd78e293e17b18d6c606ab3a19f300088d19bda9474d03ee1c85cd0c030afb9a3e68638c8b78eb33b7081360446da94ca2d512d827d42940d4656a10a8110268e586863cada7011a609cbb62cce4e1c0272ab1cfbd512b6a28fe3cf22ec0c103a9ff39a94467e7e7062464362ad55b4b867df4ab32cf97d0d4aa")
         (transaction (deserialize-transaction (hex-string->bytes transaction-data) 0)))
    (is (= 494682358388110
           (received-amount transaction address (hex-string->bytes secret-view-key)))))
  (let* ((address "9zmzEX3Ux4wMWTHesGg7jW8J6y7T5vb45RH3DZk7yHRk8G8CqtirBpY9mj1fx9RFnXfdkuj87qoF1KeKQGe2Up311XbV1ao")
         (subaddress-1 "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
         (subaddress-2 "BfZQpPkqnQkTJCmge6mpLjdQ4DRsjd2MiTAnXEywr8XRR1NbDoKgKqsFsUDZrABqJv6TZrQKnMwfTJApGMoHREXLRJJsLHT")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (transaction-data "020001020007f79503f0cb0f9415ab8b018270e845be13cb75fa484b38f0ed8c00452cb6e5a6d0a148a6643d7c41947665129745316aa803000218ce6685a1d0c85ca59bbe4053f0693ecd9c2b8fd0de4b33d0056b6d65962fb00002a95289fcab4c8f962154dab54f14806f3e0d4097884ea71eb5ee1e1bb03bc7cb0002c28e0c02e533d7d4b7605900d51f7824d186ec870f794eb0fdcd89daf3af5ca48301014bc8b6926a1676c657e0ddc97910676ec5aa456fddda2b20b3f6e9a728dc79140403274d8feba5237579c6e39294aed39ed00ffc46b34e4f9a9b50056123f10f4b0afcfc267e421edaa27f85a811fa08232b1fd6fbd683938857467cdaefb756327d3c300a6a9852f2ac7d0b6264076e57f7df9825c34fcea52e6346cba9a9c2405a03c0b0bdfa04b16e08e2a97adc4d737e854111ac8c1add1dfffb29bd61c6f7de5be3a0f745055b8b146d95fab3372a0491eee3aa488d896ea917fc4af064cf1606e9c8dec5093a69aa8af78f110bcf297bf1c16c6f9dd12cca79b5766130fc1479a4af433004dced18217899c0f4f652a427563ccbbdbbe14b5b649dc7d1749cec22d6c3c208e62629d4d7b62bf40c8386b5dcf941ea9fc2e173a09a4c84cc694ccdded94707a61e4526d0874608bf7dc58f42a89fb93294931f270cb6d5d1c4820b6a3e8f0f969dd38bd75e2d37bb23aafd9c7f87d18459186d486f82c34af0aaa79f2d157f79930aa0701aa06663c17e4945c32e80bf6637fc68006badc05e00c59a28e1c288b8ed917e974f8ddd9e2322ca2dc5be8ddff8c3c70a01c6569cf6f18eee8827872d27c2b0b019ea6e9a54b49ab17c4bc11a571b01e81b306a37b89cbf97c42a3925c6512e4da1199715275675ead73ec08693e852859304082f736b69468206c69ffaa21be14e871c1b472e593da09646c273cbf84b39ae73b684a23951fc0e40db96770fa01e13febdf4a7958989e8c6341c80707ce222bb36c0dd4eb19625823ab5775868e3ed60900b42285cb89f6fe896f710daff3e050ceb7c8370480bec85b1183cb8c5919a42ce288a8ccb4f9537629e4da624b238de1863f70bfe000613307e9f0443f04f555fadcd2576681e85d1832f8f77b33545663f956c2090e56e340303a846fd49bd1afeb9e9b729c1b14bac9a8d202c042f5f626a94cd818e446a8a606c6c49182cb6cdb68cd6b44565ee6d490382d6eeb481242c99c0367aeed8747f40a6d9a93baa463fa1692fa4870ad4d43acb680b7bfdecef8aa358659949d9fac3508dabc9c0fa4b38f2150ebcb1a48e6f6571c89c0d34615018a3d4ece7c9f17ab9215b481c26dabdaf65be1c8d4a67147198dbdaf6fb89a72f253d06bedfbb1cc1f3277fd360e37380397f5cd051bba10660d3bc546bcc2e8e0fba894ca0694326bc03fee790d2c7f624f3036b81dfa522fd3392ffa069029a82e20ebb0167d5c6cb3ac318291fb9196f83411fe47a50f4bd8dfd58edbefcb0b7af7a64569b59303a2e0c9fd644f4d29206db75952015d04870b6fa267778d4e1c4447c1ce13c7d474d3de1939975bba6c8c272fdc5a9730f338197511611ccd21ed8180cef3f47326fface014d22160d87dda29abafc1606aa99ceb482956f7d351f359555f86113c41a8b62e9f585ec22df68eebb04cd1f4d40caa9fd6ef64a7607e8a82f6f544a041614878ad2b890e2492183ce54b7a3f12363bf351c59ac4905bf43b4a9a13b891037c682eb6c4338587988ca0860ea08be09c14d6cd445940394bc4817057e0bb3f2f8fa40df3a3b0f69c18f376e67493a6e45d2bb7c8aaca7c853426edac048e8e195d984c818987bde9c77ea873873ad1c320d7263a26b8392da49fa9470bd614cb428b478392861f00295bb29ca83bb7df8c27cfdb27be66988bc74ed72ff497f19ca2124b15bf9e2e3e85a36ed79e9198c0859dc66b7b6a105ae99592776784deebae2c1dbbe662ca2349dbd79556d447ff08ae60f5f0fe45dae2df363253eb04e973eebb5ac8869f5143338a18cb272cde9fa60d78e0c06490f66fe348d65d8379671dcc5057db940bc15b8badea406d572f2d39ba2edf8b686d0919bf6db1e6502e9316d11c57c5b6243953f4e9da5872eed1e2c4c19d73ad1bc64e841860a54666eb69d057d83ce09a0f0d51a9f3d7f42b623b3e01425c5c188ed3c7113824d9a3f5ee2de40148bab6e6f4ffffc73142e201897a184a0b0ff941b3e976f5bebe51cc45b743ac2b1c0719b08f11ed72843bdc2b6836402cc6b015f54f4d6f3230c17a04fbaa2d3151d9c36ce55137ee70d309dfd1409020600b9612e75fa3c1d78a5818d34819130e43b4ddd20e7fcbb5cacbacf9f7fe42281156c79c7d7dc9cafc6e6d68b6bb6002c3573093b349263b65ed75cd56ece8b68a16afff2f0420180d5d9561d9fa5b3e33f5c141cd3483924b87b0aa0873b0082a27309629e6e001db6fc99bad41ed0d1d5a0f0adbc9cfb8bf8f0781d25d225d398058268cc2edd7ebdbf823571048e916dc5eec885b83054cd23f4e61cca6fe63efd62d74ad28f8a491eee432421dfa6d0ddbd5816764fa3a6b664bee3afd9cc40c9bd61a3286b070099439a4061f7928344424fb64f2d44b9d874fda31305ab8ecb594f88044d3190e4d929fbdd67d82efbd9e69f7b3bc8bf70fb4272b207f076e7687be8ce856c2c506801052a37cae0cbcbb2082e1859451e0a1f82c50589e5c2282ad5f6452ed8688312485b13856dc90408facf48e431890a75c353d89b107b4ebb407c122f6d6222a149227c4dfb9604ba262d15d44d8641a621ea4f046fd0299004ea228dc0db50151571568c65d6ea48bd35b2f5a8ba3cc5cecd52cc07faaa3c444628de58b485a3729eafd9d2b5c7e377df86a22a043bb4226b45bf162bdc32b76cb452f5e341ffb0e63d78b9859e245b1d55c6f3d300d4acfb0fcb42cf0421acc68ad8557c34f92da586a71a07cb5bf5206cf3a17dedf8882c0106b585934ca9a2b52bb9e7ec23d2cbbcc86610fe61b90e79b2b17d406b26f6bdd677d98b8cfb0f33a3d6ba35679550fe848217ee4ce73b846fee845435dec506556bad75c3a1805fbe3656cd4c58a8dab9022d5740f40a4df05426f7cf50302b180f26f66bb68e96da202fb404dc9a28a0885cc5473f99f32da49853ab31118bb167df14e703a9c044c69bf8e903b353c07136f7db09922509f1f307449e18721a43df434fe9797a8f0f4e65e08e08e53b6a6989c53a30546723e0ba362ec83a5f06008353a39b0c591a7a38918972823a5fe360e62cb7a230144ec21219bc81b1d039a80070b315513743184c1d9954f0019dd95e65af42c24cb7d867597c2826737ceb9b0cff745e844193a0924ac51db36804f8dcf95ae221b58e209295cdeb72cfcd9f41b051a9a3d351c9292fc251b3aec0e622f4989667265cc46a435d4bb48e2be96223cd3c652129dc6fef64a59e0ebcaa4051e40ecac047fcfe84cdee1d421e67ac8b3312f7e90717f4d978cac3e6be2c221cf1d76f688a6a2f4f2971a7e0350f409fcf36b7eee150d5f6e97af3ba8da0ad17b187756bc3af87a72e4208925f4c591a8db88de4b3141f31b750316b470ab7de125de54cd688f7b4889608b4aa14b915c226a9078f2a89441e0864dcbcc294131ff0295bc576e816492a0b5d488af6838de48e25aa0eea45da5d9756d233a5d9327fc7db6c71b8c6720d045089db820f6ae55e44f4e80b78e678366c45ca30d2b9894bf03f0a25c34bff0af5d103a3d63badde8c87aca74658afac49492c6ccb33bc432258d78bab6dbf0d1e0a9bd9c7853abe3dd66b4bb0432c68f463b17896b8d95cbca77425928925067e26e36656076c75f98f802424b78702c4528f2c772da7238bb5aa9fe01ec60dd6cac4552ac533548ab5093606f6676b05ce69961bffd1797d6c398909450a09802a652a0cf2654ca059ee8babf26595a7360a01a536df9c35610382c9ce9f0f4d61f7bb8a5661356f89fb7a9daa0375fb829b0f71bc72db9e5b04c8c6c59c0bcd4337dab6869763ab7fdfb4dda9e7836d5ca40329c3e6226d76ec0bd7d6c40408666626935abde1f7544304681792e3345ab29b34aced3152c4e08ae831990c32c13b89555ebf896f78af8b1422f15138d2415f57882717b549734212fcea036a8bcbedd5c6c641bdf603c71ee6f9219f7ba32bc93c450c438345b567fc4f0739a18137bde467bf9613839b18c3eac7697475c4921e39de7e01bdce2806b009f83e1ed63b7f42d98eaeaed25978fb731d05887622a07d449b445b4570ec03072759e068398b8f15c7f344060274f6fb0af0f5c5917fa89a1c239c5b0dd66107")
         (transaction (deserialize-transaction (hex-string->bytes transaction-data) 0)))
    (is (= 11000000000000
           (received-amount transaction subaddress-1 (hex-string->bytes secret-view-key))))
    (is (= 17000000000000
           (received-amount transaction subaddress-2 (hex-string->bytes secret-view-key))))
    (is (= 338997666200000
           (received-amount transaction address (hex-string->bytes secret-view-key))))))

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
