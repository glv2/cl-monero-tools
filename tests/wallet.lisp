;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
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
         (keys (get-wallet-keys file password)))
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

(test bruteforce-wallet-keys
  (let* ((file (asdf:system-relative-pathname "monero-tools/tests" "tests/wallet-1.keys"))
         (keys (bruteforce-wallet-keys file
                                       :threads 4
                                       :characters "0123456789"
                                       :minimum-length 5
                                       :maximum-length 6
                                       :prefix "12"
                                       :suffix "56")))
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
                                       :suffix "56")))
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
  (let ((keys (monero-tools::decode-address "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ")))
    (is (string-equal "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629"
                      (bytes->hex-string (geta keys :public-view-key)))))
  (let ((keys (monero-tools::decode-address "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7")))
    (is (string-equal "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                      (bytes->hex-string (geta keys :public-spend-key))))
    (is (string-equal "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3"
                      (bytes->hex-string (geta keys :public-view-key))))))

(test pubic-keys->address
  (flet ((public-keys->address/hex (public-spend-key public-view-key)
           (monero-tools::public-keys->address (hex-string->bytes public-spend-key)
                                               (hex-string->bytes public-view-key))))
    (is (string= "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"
                 (public-keys->address/hex "2c124acc92d2bc5999156bae00f552167a396080cd4100e4d5107bb2d102cd49"
                                           "8f5e31dbf970db6784eb5062aded5c80790f5e85667948d0bd7dd269c6722629")))
    (is (string= "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"
                 (public-keys->address/hex "3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085"
                                           "d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3")))))

(test secret-spend-key->address
  (flet ((secret-spend-key->address/hex (secret-spend-key)
           (monero-tools::secret-spend-key->address (hex-string->bytes secret-spend-key))))
    (is (string= "43HyW9SFLTmFzDLRgYmeLZ4m4SW7w3G8XfGxfjW79oh6DJdDdwybRJJJKGS3SWsoH5NVMBaXyzty5bv3QNRarqDw5iPfNJJ"
                 (secret-spend-key->address/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306")))
    (is (string= "43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7"
                 (secret-spend-key->address/hex "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a")))))

(test secret-key->mnemonic-seed
  (flet ((secret-key->mnemonic-seed/hex (secret-key language)
           (secret-key->mnemonic-seed (hex-string->bytes secret-key) language)))
    (is (string-equal "dedicated dubbed coexist having damp ember feline inquest september nobody alley binocular lopped moat agreed wayside gotten bays layout nail vixen imagine weird yahoo moat"
                      (secret-key->mnemonic-seed/hex "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306" :english)))
    (is (string-equal "brave visuel version tango davantage baobab quinze essai peau tellement balcon brevet tasse ordinaire rhume lueur oreille version stock agonie salon scoop rouge seuil peau"
                      (secret-key->mnemonic-seed/hex "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a" :french)))))

(test mnemonic-seed->secret-key
  (flet ((mnemonic-seed->secret-key/hex (mnemonic-seed language)
           (bytes->hex-string (mnemonic-seed->secret-key mnemonic-seed language))))
    (is (string-equal "f61b1df1b8bc17126ebd95587494fb128a39217dd468e6bea57f2263626c1306"
                      (mnemonic-seed->secret-key/hex "dedicated dubbed coexist having damp ember feline inquest september nobody alley binocular lopped moat agreed wayside gotten bays layout nail vixen imagine weird yahoo moat" :english)))
    (is (string-equal "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a"
                      (mnemonic-seed->secret-key/hex "brave visuel version tango davantage baobab quinze essai peau tellement balcon brevet tasse ordinaire rhume lueur oreille version stock agonie salon scoop rouge seuil peau" :french)))))
