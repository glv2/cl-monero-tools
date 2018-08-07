;;;; This file is part of monero-tools
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools/tests)


(in-suite wallet-tests)

#|
Info about 2/3 multisig wallet
------------------------------
file: wallet-1.keys
password: 123456
mnemonic seed language: english
mnemonic seed: maze olympics madness digit antics heron daily mayor whole juggled bowling ulcers badge ecstatic needed noises zodiac going elite guys cement cider cistern cycling going
address: 48PxjvS2nzN4PsTQmhYhcM5Eq5TFWeMhK9S1KQ53ykLkWAaGoHVLXDPNvLCAqV6h1FYQ9c5kQ8ds9AcUYXYx4JNFUm5JiBF
secret spend key: ddf53de98c2cde52a5daac71a7464dc4f46d7d54afad4259952ae3c97f663305
public spend key: b2bcadb323572b144b1242341a3d3a1957049df4a13fca32680b806a3d99a5ae
secret view key: 6f5631bfa0a7a4719cd2ddf3acf9ab26a93c88dc5560ebf8e426a544ccad8803
public view key: 5dd43c9a09e1be830c517dad4040f6bbb9174267550c543976d6ef8ad80d7cf6
blinded secret view key: 37086110d56edb0d244128eee6980255f427acca9b965fe4a8a26d17ff8be005
secret signer key: 2198d1a8f259eee41ef087eb48559edbd5841cce739709f79767adb3fbd25104
public signer key: eafbc2265dca6fca729f038d00a408f03624f039ea10a9be1ef3d19aaed4e856
multisig info: MultisigV1ACtQwqtBN6x74iS7stvtsWhqcLBXBvy71VCy1WVGE9UGgJdkYqo47wXLAyBkxpieFRA4GWgDAGtNh6BH5ZZenvRTGUVMMQKszEjAPnL2A5hyxVTymHoMaPE2nj82hnS2evNaSXj4yh8LKfa2o1zE9QMaNp2PtGvm6TUdwjb2bzjoBrhg
multisig extra info: MultisigxV1B2HV6651uKBCHqqUYg3aQwaKrpm8Vdbu1ApYBeyxcEnSV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3AiDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBPVbdRfcf9EC6XTrT4UwDtfNchktG4LErSCHxrq2tZtirCFED5B41YoxtcoVJjUoaFdWDk9DCW3BXXZUzqQZeLLriK
multisig keys: ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302
               f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07
multisig public keys: a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71
                      f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a
multisig secret signer key: a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09
multisig public signer key: 3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47
multisig seed: 0200000003000000a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c073beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4

file: wallet-2.keys
password: 123456
mnemonic seed language: english
mnemonic seed: hounded touchy august zodiac python sapling females fibula shelter fountain corrode value comb stunning claim auburn voyage winter business awkward gasp hydrogen veteran violin voyage
address: 46gdkg6bsesKKE4pSx8kKgDjb6TJ9L1DJPFZYtcRRsRzHveDSLoNzSjAdAA8Ro6KeT1W5zUbovUVJGFpR6ubqXW3BuFyKCB
secret spend key: 88a82e356a24ec0ec1dd497a3264c1c28741905781aecf051f89ec3dc962d101
public spend key: 858bc0c44c9b5c6d7e5de42c6fc9af4c221c3af2a8d8e18507c6278aefc11165
secret view key: 8bc2f1c91767c32404b5fc906122ee5a22efb8c5c8e229477850069fda334c04
public view key: 2fe5d377480d203988dcdd13a296ec02ff8861fd0fc0dd5b34de998485eb2c60
blinded secret view key: a8d18e7ce2e4de4c6213b9e883280ab0d6ee0c4640942151270125ffa71b500f
secret signer key: f4d31e959d1dfe34619da70dbecec62fb89c29125f094cc1fcc67fd0d6d60909
public signer key: 1892038bd30631aa5f0e056ae47f87b2ed9d72e32dc20b645ab0171d52884f42
multisig info: MultisigV1VEkbitipY51HQUH56z8Pu9cx6ATQD4Cvk7XPkwqkDbfU57N5FsBRiN5Gu9ikLqv44qgkAUxAmw739GAnaLS1MAzMa3kjv1m72PwfVM1z3JEeBpdSWRsSXHpEHbXwSiPywurujVYitznC8aRgqnHu7W1Zs8fbpirY1bFH4CqtBx4iedJe
multisig extra info: MultisigxV1dKNJHKg34Yja8Mef3fkswXAszjskHbs7PVAReAG74PMDV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3A1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQE1YJed5NXQWpji7S4cAHq53cEtkVjydF1ALHYzMs1F2gYd2whUWuFJf4ZPonVdg4hb1hc8k4u79WA1PpS9y37nco6
multisig keys: ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302
               b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08
multisig public keys: a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71
                      04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b
multisig secret signer key: 627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400b
multisig public signer key: d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4
multisig seed: 0200000003000000627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400baeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb49743beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47

file: wallet-3.keys
password: 123456
mnemonic seed language: english
mnemonic seed: shackles newt tadpoles gags pencil vowels revamp aided podcast niche foyer hitched cupcake damp mops taken agreed tumbling itinerary mops hospital excess elbow empty revamp
address: 45vbpu87oHYZjoKndmjGYrEKUnqqaP9qE9d4CYRKk7gzezD5gUqhh9s2om1rmWg1FYMkVdpvMk5hACozjD6Z3odP136DTtB
secret spend key: 02dea042ab01454d9ca369a454cb0613c6f2c35d9009c3daf959b3d839570b02
public spend key: 7183159c33b0d7c3ba278d37ed48634fa0722e5a5a580d338ba0bb1029aeffe3
secret view key: 64bf5bdbbb82c6d084b1ae8eff4b0710b372e53465c0c9d90db1bbc8dcbb9c06
public view key: 1b051d83f428820acc69d0534f7ff37c0e34f60cf7cb81469be46efc7f9c2600
blinded secret view key: 8b9bc92fc3849735654c8e6be3fea89db96b8724b90cec21d0fea2485b062506
secret signer key: aede2ddc27f47417d81832ae44f917c9d33ad3ae5f4cadb2a9b8e8c55e7c9502
public signer key: 49022814b64909a8c20078877610d80add01fbf6ef9f55fc7c93b2b2f9029122
multisig info: MultisigV1QMNubqf2Zp8HwjCr8qVT32Y1onHiaatCYbxWam4oCdm7DDGqwJDdmzFZT4Xim7nEvDdy422avHWMqMqZ3MH8BCGqGbg2f3xo8YoDvX4kEFYyb9Jd6H5z7DGToJBnLrtpZoqfXzryyUwpGzWEZ2qkVtKipgQ94RWETo1z6QDEtu8SDVN3
multisig extra info: MultisigxV1gwsG1Ys15VfFQRNUd1LeNfYXzC9XtmTvJLcpxPzhRBF1iDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBP1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQEb9VH6YrT69raSwU7fjnQuyMD5vPeKn9Hqe4VEGz2qdaNWvMx3cXuzqF7xNfbSAZG4JVqesLiFNRc52hV1pDt5JML
multisig keys: f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07
               b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08
multisig public keys: f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a
                      04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b
multisig secret signer key: a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0f
multisig public signer key: eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974
multisig seed: 0200000003000000a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0faeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a43beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47

multisig address: 48F6EyTxrPmbxN15svcgLZJYJUTZ8qvfHWv7aoJGfDBYLGNWLsAjmVNBR7tQgVHd7ZS5A9V43JM3v6dAtpt19nTQC1RxFqX
multisig public spend key: aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373
multisig secret view key: 7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b
multisig public view key: 2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61
|#

(test make-multisig-info
  (flet ((string=-no-sig (x y)
           (string= x y :end1 (- (length x) 128) :end2 (- (length y) 128))))
    (let ((secret-view-key "6f5631bfa0a7a4719cd2ddf3acf9ab26a93c88dc5560ebf8e426a544ccad8803")
          (secret-spend-key "ddf53de98c2cde52a5daac71a7464dc4f46d7d54afad4259952ae3c97f663305"))
      (is (string=-no-sig "MultisigV1ACtQwqtBN6x74iS7stvtsWhqcLBXBvy71VCy1WVGE9UGgJdkYqo47wXLAyBkxpieFRA4GWgDAGtNh6BH5ZZenvRTGUVMMQKszEjAPnL2A5hyxVTymHoMaPE2nj82hnS2evNaSXj4yh8LKfa2o1zE9QMaNp2PtGvm6TUdwjb2bzjoBrhg"
                          (make-multisig-info (hex-string->bytes secret-view-key)
                                              (hex-string->bytes secret-spend-key)))))
    (let ((secret-view-key "8bc2f1c91767c32404b5fc906122ee5a22efb8c5c8e229477850069fda334c04")
          (secret-spend-key "88a82e356a24ec0ec1dd497a3264c1c28741905781aecf051f89ec3dc962d101"))
      (is (string=-no-sig "MultisigV1VEkbitipY51HQUH56z8Pu9cx6ATQD4Cvk7XPkwqkDbfU57N5FsBRiN5Gu9ikLqv44qgkAUxAmw739GAnaLS1MAzMa3kjv1m72PwfVM1z3JEeBpdSWRsSXHpEHbXwSiPywurujVYitznC8aRgqnHu7W1Zs8fbpirY1bFH4CqtBx4iedJe"
                          (make-multisig-info (hex-string->bytes secret-view-key)
                                              (hex-string->bytes secret-spend-key)))))
    (let ((secret-view-key "64bf5bdbbb82c6d084b1ae8eff4b0710b372e53465c0c9d90db1bbc8dcbb9c06")
          (secret-spend-key "02dea042ab01454d9ca369a454cb0613c6f2c35d9009c3daf959b3d839570b02"))
      (is (string=-no-sig "MultisigV1QMNubqf2Zp8HwjCr8qVT32Y1onHiaatCYbxWam4oCdm7DDGqwJDdmzFZT4Xim7nEvDdy422avHWMqMqZ3MH8BCGqGbg2f3xo8YoDvX4kEFYyb9Jd6H5z7DGToJBnLrtpZoqfXzryyUwpGzWEZ2qkVtKipgQ94RWETo1z6QDEtu8SDVN3"
                          (make-multisig-info (hex-string->bytes secret-view-key)
                                              (hex-string->bytes secret-spend-key)))))))

(test decode-multisig-info
  (let ((multisig-info (decode-multisig-info "MultisigV1ACtQwqtBN6x74iS7stvtsWhqcLBXBvy71VCy1WVGE9UGgJdkYqo47wXLAyBkxpieFRA4GWgDAGtNh6BH5ZZenvRTGUVMMQKszEjAPnL2A5hyxVTymHoMaPE2nj82hnS2evNaSXj4yh8LKfa2o1zE9QMaNp2PtGvm6TUdwjb2bzjoBrhg")))
    (is (string-equal "37086110d56edb0d244128eee6980255f427acca9b965fe4a8a26d17ff8be005"
                      (bytes->hex-string (geta multisig-info :secret-view-key))))
    (is (string-equal "eafbc2265dca6fca729f038d00a408f03624f039ea10a9be1ef3d19aaed4e856"
                      (bytes->hex-string (geta multisig-info :public-signer-key)))))
  (let ((multisig-info (decode-multisig-info "MultisigV1VEkbitipY51HQUH56z8Pu9cx6ATQD4Cvk7XPkwqkDbfU57N5FsBRiN5Gu9ikLqv44qgkAUxAmw739GAnaLS1MAzMa3kjv1m72PwfVM1z3JEeBpdSWRsSXHpEHbXwSiPywurujVYitznC8aRgqnHu7W1Zs8fbpirY1bFH4CqtBx4iedJe")))
    (is (string-equal "a8d18e7ce2e4de4c6213b9e883280ab0d6ee0c4640942151270125ffa71b500f"
                      (bytes->hex-string (geta multisig-info :secret-view-key))))
    (is (string-equal "1892038bd30631aa5f0e056ae47f87b2ed9d72e32dc20b645ab0171d52884f42"
                      (bytes->hex-string (geta multisig-info :public-signer-key)))))
  (let ((multisig-info (decode-multisig-info "MultisigV1QMNubqf2Zp8HwjCr8qVT32Y1onHiaatCYbxWam4oCdm7DDGqwJDdmzFZT4Xim7nEvDdy422avHWMqMqZ3MH8BCGqGbg2f3xo8YoDvX4kEFYyb9Jd6H5z7DGToJBnLrtpZoqfXzryyUwpGzWEZ2qkVtKipgQ94RWETo1z6QDEtu8SDVN3")))
    (is (string-equal "8b9bc92fc3849735654c8e6be3fea89db96b8724b90cec21d0fea2485b062506"
                      (bytes->hex-string (geta multisig-info :secret-view-key))))
    (is (string-equal "49022814b64909a8c20078877610d80add01fbf6ef9f55fc7c93b2b2f9029122"
                      (bytes->hex-string (geta multisig-info :public-signer-key))))))

(test make-multisig-extra-info
  (flet ((string=-no-sig (x y)
           (string= x y :end1 (- (length x) 128) :end2 (- (length y) 128))))
    (let ((multisig-keys (map 'vector
                              #'hex-string->bytes
                              #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                                "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"))))
      (is (string=-no-sig "MultisigxV1B2HV6651uKBCHqqUYg3aQwaKrpm8Vdbu1ApYBeyxcEnSV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3AiDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBPVbdRfcf9EC6XTrT4UwDtfNchktG4LErSCHxrq2tZtirCFED5B41YoxtcoVJjUoaFdWDk9DCW3BXXZUzqQZeLLriK"
                          (make-multisig-extra-info multisig-keys))))
    (let ((multisig-keys (map 'vector
                              #'hex-string->bytes
                              #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                                "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
      (is (string=-no-sig "MultisigxV1dKNJHKg34Yja8Mef3fkswXAszjskHbs7PVAReAG74PMDV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3A1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQE1YJed5NXQWpji7S4cAHq53cEtkVjydF1ALHYzMs1F2gYd2whUWuFJf4ZPonVdg4hb1hc8k4u79WA1PpS9y37nco6"
                          (make-multisig-extra-info multisig-keys))))
    (let ((multisig-keys (map 'vector
                              #'hex-string->bytes
                              #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                                "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
      (is (string=-no-sig "MultisigxV1gwsG1Ys15VfFQRNUd1LeNfYXzC9XtmTvJLcpxPzhRBF1iDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBP1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQEb9VH6YrT69raSwU7fjnQuyMD5vPeKn9Hqe4VEGz2qdaNWvMx3cXuzqF7xNfbSAZG4JVqesLiFNRc52hV1pDt5JML"
                          (make-multisig-extra-info multisig-keys))))))

(test decode-multisig-extra-info
  (let ((multisig-extra-info (decode-multisig-extra-info "MultisigxV1B2HV6651uKBCHqqUYg3aQwaKrpm8Vdbu1ApYBeyxcEnSV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3AiDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBPVbdRfcf9EC6XTrT4UwDtfNchktG4LErSCHxrq2tZtirCFED5B41YoxtcoVJjUoaFdWDk9DCW3BXXZUzqQZeLLriK")))
    (is (string-equal "3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"
                      (bytes->hex-string (geta multisig-extra-info :public-signer-key))))
    (is (equalp #("a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71"
                  "f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a")
                (map 'vector #'bytes->hex-string (geta multisig-extra-info :multisig-public-keys)))))
  (let ((multisig-extra-info (decode-multisig-extra-info "MultisigxV1dKNJHKg34Yja8Mef3fkswXAszjskHbs7PVAReAG74PMDV163PF3KF4HjEegBM9Y36iQJtbd5W4BDJ1BhyGCuhE3A1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQE1YJed5NXQWpji7S4cAHq53cEtkVjydF1ALHYzMs1F2gYd2whUWuFJf4ZPonVdg4hb1hc8k4u79WA1PpS9y37nco6")))
    (is (string-equal "d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                      (bytes->hex-string (geta multisig-extra-info :public-signer-key))))
    (is (equalp #("a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71"
                  "04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b")
                (map 'vector #'bytes->hex-string (geta multisig-extra-info :multisig-public-keys)))))
  (let ((multisig-extra-info (decode-multisig-extra-info "MultisigxV1gwsG1Ys15VfFQRNUd1LeNfYXzC9XtmTvJLcpxPzhRBF1iDf6VpWP5gE4QcwTTjEbCTgS1xpWwhYs9gh6oJdTrFBP1p1sHTeyeHGb4pu6x9ScKvAjuEzwgAe8D8hMyhWjQyQEb9VH6YrT69raSwU7fjnQuyMD5vPeKn9Hqe4VEGz2qdaNWvMx3cXuzqF7xNfbSAZG4JVqesLiFNRc52hV1pDt5JML")))
    (is (string-equal "eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                      (bytes->hex-string (geta multisig-extra-info :public-signer-key))))
    (is (equalp #("f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a"
                  "04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b")
                (map 'vector #'bytes->hex-string (geta multisig-extra-info :multisig-public-keys))))))

(test make-multisig-seed
  (let ((threshold 2)
        (total 3)
        (secret-spend-key (hex-string->bytes "a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09"))
        (public-spend-key (hex-string->bytes "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"))
        (secret-view-key (hex-string->bytes "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"))
        (public-view-key (hex-string->bytes "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"))
        (multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07")))
        (signers (map 'vector
                      #'hex-string->bytes
                      #("3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"
                        "eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                        "d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"))))
    (is (string-equal "0200000003000000a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c073beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                      (bytes->hex-string (make-multisig-seed threshold
                                                             total
                                                             secret-spend-key
                                                             public-spend-key
                                                             secret-view-key
                                                             public-view-key
                                                             multisig-keys
                                                             signers)))))
  (let ((threshold 2)
        (total 3)
        (secret-spend-key (hex-string->bytes "627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400b"))
        (public-spend-key (hex-string->bytes "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"))
        (secret-view-key (hex-string->bytes "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"))
        (public-view-key (hex-string->bytes "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"))
        (multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")))
        (signers (map 'vector
                      #'hex-string->bytes
                      #("d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                        "eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                        "3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"))))
    (is (string-equal "0200000003000000627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400baeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb49743beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"
                      (bytes->hex-string (make-multisig-seed threshold
                                                             total
                                                             secret-spend-key
                                                             public-spend-key
                                                             secret-view-key
                                                             public-view-key
                                                             multisig-keys
                                                             signers)))))
  (let ((threshold 2)
        (total 3)
        (secret-spend-key (hex-string->bytes "a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0f"))
        (public-spend-key (hex-string->bytes "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"))
        (secret-view-key (hex-string->bytes "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"))
        (public-view-key (hex-string->bytes "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"))
        (multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")))
        (signers (map 'vector
                      #'hex-string->bytes
                      #("eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                        "d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                        "3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"))))
    (is (string-equal "0200000003000000a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0faeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a43beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"
                      (bytes->hex-string (make-multisig-seed threshold
                                                             total
                                                             secret-spend-key
                                                             public-spend-key
                                                             secret-view-key
                                                             public-view-key
                                                             multisig-keys
                                                             signers))))))

(test decode-multisig-seed
  (let* ((info (decode-multisig-seed "0200000003000000a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c073beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"))
         (threshold (geta info :threshold))
         (total (geta info :total))
         (secret-spend-key (bytes->hex-string (geta info :secret-spend-key)))
         (public-spend-key (bytes->hex-string (geta info :public-spend-key)))
         (secret-view-key (bytes->hex-string (geta info :secret-view-key)))
         (public-view-key (bytes->hex-string (geta info :public-view-key)))
         (multisig-keys (map 'vector #'bytes->hex-string (geta info :multisig-keys)))
         (signers (map 'vector #'bytes->hex-string (geta info :signers))))
    (is (= 2 threshold))
    (is (= 3 total))
    (is (string-equal "a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09"
                      secret-spend-key))
    (is (string-equal "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"
                      public-spend-key))
    (is (string-equal "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"
                      secret-view-key))
    (is (string-equal "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"
                      public-view-key))
    (is (equalp #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                  "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07")
                multisig-keys))
    (is (equalp #("3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"
                  "eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                  "d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4")
                signers)))
  (let* ((info (decode-multisig-seed "0200000003000000627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400baeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb49743beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"))
         (threshold (geta info :threshold))
         (total (geta info :total))
         (secret-spend-key (bytes->hex-string (geta info :secret-spend-key)))
         (public-spend-key (bytes->hex-string (geta info :public-spend-key)))
         (secret-view-key (bytes->hex-string (geta info :secret-view-key)))
         (public-view-key (bytes->hex-string (geta info :public-view-key)))
         (multisig-keys (map 'vector #'bytes->hex-string (geta info :multisig-keys)))
         (signers (map 'vector #'bytes->hex-string (geta info :signers))))
    (is (= 2 threshold))
    (is (= 3 total))
    (is (string-equal "627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400b"
                      secret-spend-key))
    (is (string-equal "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"
                      public-spend-key))
    (is (string-equal "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"
                      secret-view-key))
    (is (string-equal "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"
                      public-view-key))
    (is (equalp #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                  "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")
                multisig-keys))
    (is (equalp #("d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                  "eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                  "3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47")
                signers)))
  (let* ((info (decode-multisig-seed "0200000003000000a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0faeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb93737da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a43beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47"))
         (threshold (geta info :threshold))
         (total (geta info :total))
         (secret-spend-key (bytes->hex-string (geta info :secret-spend-key)))
         (public-spend-key (bytes->hex-string (geta info :public-spend-key)))
         (secret-view-key (bytes->hex-string (geta info :secret-view-key)))
         (public-view-key (bytes->hex-string (geta info :public-view-key)))
         (multisig-keys (map 'vector #'bytes->hex-string (geta info :multisig-keys)))
         (signers (map 'vector #'bytes->hex-string (geta info :signers))))
    (is (= 2 threshold))
    (is (= 3 total))
    (is (string-equal "a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0f"
                      secret-spend-key))
    (is (string-equal "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"
                      public-spend-key))
    (is (string-equal "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"
                      secret-view-key))
    (is (string-equal "2d90d31aa9903d3e45b17fbc849b6c95e4d08c3694b721219f1a08208bd7af61"
                      public-view-key))
    (is (equalp #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                  "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")
                multisig-keys))
    (is (equalp #("eed245dba37aae0e561d5b0ecd5f11d4bc87ea454eb8aa0b75495dc2a5fb4974"
                  "d91f8b4ba3bed51cc60d67ba59611dba3b1067264432812aa85f60bc784b95a4"
                  "3beb20d20b3dc70e4380324667fd63f0c73cf34ea1a1c6d03ab527e86aa8ab47")
                signers))))
