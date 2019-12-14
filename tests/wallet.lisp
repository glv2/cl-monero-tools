;;;; This file is part of monero-tools
;;;; Copyright 2016-2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite wallet-tests
  :description "Unit tests for wallet functions."
  :in monero-tools-tests)

(in-suite wallet-tests)

#|
Info about wallet-1
-------------------
file: wallet-1.keys
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
file: wallet-2.keys
password: bhunjivgy
mnemonic seed language: french
mnemonic seed: brave visuel version tango davantage baobab quinze essai peau tellement balcon brevet tasse ordinaire rhume lueur oreille version stock agonie salon scoop rouge seuil peau
address: 43oErH6q2FfVkVBXrkQt3yfYmmZN3iseWfwet7TyeXmRPPGFcVFffzpSp92tKSUGKF4yKNh5LRLLh8fEaor4Zx3ySdMJNm7
secret spend key: b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a
public spend key: 3962d1d1e47c06abe2314ed2849c40e67651a449144a2fe8d21e1146653fd085
secret view key: 9197ed5871e7306e49f9d604dc3d9c64aa678bb0736b80f09158ab5ce9305e0c
public view key: d302c0849107819a5301b91e08928617bdadcf303b05122dca9adc24d80238e3
testnet: no


Info about 2/3 multisig wallets
-------------------------------
file: multisig-wallet-1.keys
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

file: multisig-wallet-2.keys
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

file: multisig-wallet-3.keys
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


Info about 2/2 multisig wallets
-------------------------------
file: multisig-wallet-4.keys
password: 123456
mnemonic seed language: english
mnemonic seed: space android leopard building goldfish because dwelt aloof victim tutor merger viking iris hotel also together relic roped loyal exquisite audio awakened asked blender merger
address: 49djRyZhy4m2fyo3JBZU8jQ1dtEN9qHgcUDk8dpnxxkd82mvFPhJqjQ7UcuBEQiRYyS42NtwYD1MieYUyEMA3Z1nUqs9C5t
secret spend key: 16f065708dc3f9bf6095ade6867dbd68da2968a668e67d05f14b9dcd2986f90b
public spend key: d363fc369042fa09ff1ee3215de34c8992d5332e90da19a2bc6e884a26f8e62a
secret view key: 5e41a1e789066174019f1c82fab08cb7b7ca96965d972849c06258b79224b704
public view key: 087e269921c94326b7f66c76ef721695c6e4486cbcb181e07426e6d4d3914df6
blinded secret view key: f78e43b21da28affdaabcd00189c7f6a19458b2a695fe90b670ba8197c628b08
multisig secret signer key: 367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900
multisig public signer key: ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8
multisig info: MultisigV1iQbqPBYtGQ6daPFHgoANND5EAfofFeaivJEfuRpabfDybWmLZJ5cKU6WYKLxzEaPwLGKhuDaB6qUQhLbJ8yTypx7FXx47Zc4uwgaouaxniTnbuaY7rv9mCAA9KVy7Q7UPiarSTn2exrQPti3TPSPmwQu89MTCfkpxGq7pR6bA4KwDZpe
multisig seed: 0200000002000000367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c007969002297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7cedce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8

file: multisig-wallet-5.keys
password: 123456
mnemonic seed language: english
mnemonic seed: spout fierce thaw vials bowling hiding lion left bygones august renting woken foggy prying haystack daytime cogs warped biscuit knife austere altitude dubbed eldest altitude
address: 4A8PXSb4gttCLcbL3d4MdoSSsMkfy9Z5JAwcgrLo67SBgTQ9rWk6LTiAmbZYBfQBDPYj7Zu5LkKTic788qWZqWVHKkoeqjx
secret spend key: 6919108e1aa3f4402c670aa762e4554400fc3db0c26c12cc31f9e89b1b93a506
public spend key: e06db8373af2f543c955f86af157de9821b84ef65566b93b6fec5799a119d4eb
secret view key: 0724632717bc4d9195c7d0b8c19565adaa88b8e8d62df899559eeef1d74d3e0c
public view key: e31113ef9b84913a67870564e5d82ebdad8ad6587860fdd1e1ea925fdfa49ca6
blinded secret view key: b3e6f3b9b75b5fc2097f076c54cbd66d0d8a2b4c01723dd3e48bf85c1b38e409
multisig secret signer key: 5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c
multisig public signer key: 156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced
multisig info: MultisigV1X6H2P2QJ8Y92b8FrCQ2Nk43GMS47gHy5UfEC1eEBm9Jp4asf3n68DeYeGx5d4pnk77VFte1eXtwTVBHcaudJktXnfTxL4mA4G3baHFn9ncRvQSXACjyyLriJgbtWeai6kdoiRxx7F6p42AqjSJLoaExLhrRwTjbKrDkdejLj2s4SyadG
multisig seed: 02000000020000005b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c95b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750cce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced

multisig address: 42w9C4zK7TVbheCa4DiBDwJuKPLfLU5xEZt9RbD6Mn4AhHjyt3sQYNJiTUyriLSwTU9dEzYHrXWsj2dA646xyia4PhRpXLs
multisig public spend key: 2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0
multisig secret view key: bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02
multisig public view key: deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9
|#

(test get-wallet-keys
  (let* ((file (data-file-path "wallet-1.keys"))
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
  (let* ((file (data-file-path "wallet-2.keys"))
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
  (let* ((file (data-file-path "wallet-1.keys"))
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
  (let* ((file (data-file-path "wallet-1.keys"))
         (keys (bruteforce-wallet-keys file
                                       :threads 2
                                       :characters "abcdefgh"
                                       :minimum-length 5
                                       :maximum-length 6
                                       :prefix "12"
                                       :suffix "56"
                                       :chacha8 t)))
    (is (null keys)))
  (let* ((file (data-file-path "wallet-1.keys"))
         (dictionary (data-file-path "dictionary.txt"))
         (keys (bruteforce-wallet-keys file :threads 4 :dictionary-file dictionary)))
    (is (null keys)))
  (let* ((file (data-file-path "wallet-2.keys"))
         (keys (bruteforce-wallet-keys file
                                       :minimum-length 9
                                       :maximum-length 9
                                       :prefix "bhunjivg")))
    (is (string= "bhunjivgy"
                 (geta keys :password)))
    (is (string-equal "b50710fdc751efdd2602635a0e271d0af6744a2bf58ca15a138dd6ca5ad78d0a"
                      (bytes->hex-string (geta keys :secret-spend-key)))))
  (let* ((file (data-file-path "wallet-2.keys"))
         (dictionary (data-file-path "dictionary.txt"))
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
        (file-1 (data-file-path "message-1.dat"))
        (file-2 (data-file-path "message-2.dat")))
    (is-true (valid-file-signature-p file-1 address "SigV1Nh4zdYvLpH4Q8Mgi9CAnf5FgK5ExjEmS1S8cQVAt4Qgs8Jx4GohCV9z8qSgfK1CzymMsCYuBELjjkLGUa6n6tBit"))
    (is-true (valid-file-signature-p file-2 address "SigV1bcxztfKHU3hRaYrEiWjh3s5KCzDwL7bWKDTWzger6dSUD8pSjtkn7G8SpWjK7obDyJMe7JEx7okYYe23JFXWDhm6"))))

(test sign-file
  (let ((secret-spend-key (hex-string->bytes "d551999169b794459b4c8dc7da177067213a0eb2dd75cacf19e0fbc27dfc320e"))
        (address "9trf6E3P7r3asxsaoRFpW3RYJXBC4DWKKN9EN4oAif48SH4u4V57zKQMERtJ2KRxTpDJMnpkSKGs29PsoHMb8zgKLPfF1NQ")
        (file-1 (data-file-path "message-1.dat"))
        (file-2 (data-file-path "message-2.dat")))
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
         (subaddress-1 "Bcni4ZAM42o4QcYG7fCFwX8GijijG43d5YnafUhSp7D8iXZ5K4Qxbr4SEKZiHqfFFnDbn6Eq5MDY6iMPhtDJb6sSEqxHNSV")
         (subaddress-2 "BfZQpPkqnQkTJCmge6mpLjdQ4DRsjd2MiTAnXEywr8XRR1NbDoKgKqsFsUDZrABqJv6TZrQKnMwfTJApGMoHREXLRJJsLHT")
         (secret-view-key "fabfcc6b35389437dd69ace7d3280f794f4e27e993e1ada5726a3fd84c9bbb00")
         (transaction-data (load-hex-data "txn-55b0833a74d6b933d3c97a74bebd2b2d0926e8a091a51e170fa5e7ae26019a9b.hex"))
         (transaction (deserialize-transaction (hex-string->bytes transaction-data) 0)))
    (is (= 11000000000000
           (received-amount transaction subaddress-1 (hex-string->bytes secret-view-key))))
    (is (= 17000000000000
           (received-amount transaction subaddress-2 (hex-string->bytes secret-view-key))))
    (is (= 11603063785325
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
        (file1 (data-file-path "qr1.png"))
        (file2 (data-file-path "qr2.png"))
        (file3 (data-file-path "qr3.png"))
        (file4 (data-file-path "qr4.png")))
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
        (file (data-file-path "qr0.png")))
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
                                              (hex-string->bytes secret-spend-key)))))
    (let ((secret-view-key "5e41a1e789066174019f1c82fab08cb7b7ca96965d972849c06258b79224b704")
          (secret-spend-key "16f065708dc3f9bf6095ade6867dbd68da2968a668e67d05f14b9dcd2986f90b"))
      (is (string=-no-sig "MultisigV1iQbqPBYtGQ6daPFHgoANND5EAfofFeaivJEfuRpabfDybWmLZJ5cKU6WYKLxzEaPwLGKhuDaB6qUQhLbJ8yTypx7FXx47Zc4uwgaouaxniTnbuaY7rv9mCAA9KVy7Q7UPiarSTn2exrQPti3TPSPmwQu89MTCfkpxGq7pR6bA4KwDZpe"
                          (make-multisig-info (hex-string->bytes secret-view-key)
                                              (hex-string->bytes secret-spend-key)))))
    (let ((secret-view-key "0724632717bc4d9195c7d0b8c19565adaa88b8e8d62df899559eeef1d74d3e0c")
          (secret-spend-key "6919108e1aa3f4402c670aa762e4554400fc3db0c26c12cc31f9e89b1b93a506"))
      (is (string=-no-sig "MultisigV1X6H2P2QJ8Y92b8FrCQ2Nk43GMS47gHy5UfEC1eEBm9Jp4asf3n68DeYeGx5d4pnk77VFte1eXtwTVBHcaudJktXnfTxL4mA4G3baHFn9ncRvQSXACjyyLriJgbtWeai6kdoiRxx7F6p42AqjSJLoaExLhrRwTjbKrDkdejLj2s4SyadG"
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
                      (bytes->hex-string (geta multisig-info :public-signer-key)))))
  (let ((multisig-info (decode-multisig-info "MultisigV1iQbqPBYtGQ6daPFHgoANND5EAfofFeaivJEfuRpabfDybWmLZJ5cKU6WYKLxzEaPwLGKhuDaB6qUQhLbJ8yTypx7FXx47Zc4uwgaouaxniTnbuaY7rv9mCAA9KVy7Q7UPiarSTn2exrQPti3TPSPmwQu89MTCfkpxGq7pR6bA4KwDZpe")))
    (is (string-equal "f78e43b21da28affdaabcd00189c7f6a19458b2a695fe90b670ba8197c628b08"
                      (bytes->hex-string (geta multisig-info :secret-view-key))))
    (is (string-equal "ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"
                      (bytes->hex-string (geta multisig-info :public-signer-key)))))
  (let ((multisig-info (decode-multisig-info "MultisigV1X6H2P2QJ8Y92b8FrCQ2Nk43GMS47gHy5UfEC1eEBm9Jp4asf3n68DeYeGx5d4pnk77VFte1eXtwTVBHcaudJktXnfTxL4mA4G3baHFn9ncRvQSXACjyyLriJgbtWeai6kdoiRxx7F6p42AqjSJLoaExLhrRwTjbKrDkdejLj2s4SyadG")))
    (is (string-equal "b3e6f3b9b75b5fc2097f076c54cbd66d0d8a2b4c01723dd3e48bf85c1b38e409"
                      (bytes->hex-string (geta multisig-info :secret-view-key))))
    (is (string-equal "156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"
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
                                                             signers)))))
  (let ((threshold 2)
        (total 2)
        (secret-spend-key (hex-string->bytes "367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900"))
        (public-spend-key (hex-string->bytes "2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0"))
        (secret-view-key (hex-string->bytes "bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02"))
        (public-view-key (hex-string->bytes "deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9"))
        (multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900")))
        (signers (map 'vector
                      #'hex-string->bytes
                      #("156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"
                        "ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"))))
    (is (string-equal "0200000002000000367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c007969002297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7cedce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"
                      (bytes->hex-string (make-multisig-seed threshold
                                                             total
                                                             secret-spend-key
                                                             public-spend-key
                                                             secret-view-key
                                                             public-view-key
                                                             multisig-keys
                                                             signers)))))
  (let ((threshold 2)
        (total 2)
        (secret-spend-key (hex-string->bytes "5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c"))
        (public-spend-key (hex-string->bytes "2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0"))
        (secret-view-key (hex-string->bytes "bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02"))
        (public-view-key (hex-string->bytes "deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9"))
        (multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c")))
        (signers (map 'vector
                      #'hex-string->bytes
                      #("ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"
                        "156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"))))
    (is (string-equal "02000000020000005b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c95b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750cce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"
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
                signers)))
  (let* ((info (decode-multisig-seed "0200000002000000367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c007969002297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7cedce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"))
         (threshold (geta info :threshold))
         (total (geta info :total))
         (secret-spend-key (bytes->hex-string (geta info :secret-spend-key)))
         (public-spend-key (bytes->hex-string (geta info :public-spend-key)))
         (secret-view-key (bytes->hex-string (geta info :secret-view-key)))
         (public-view-key (bytes->hex-string (geta info :public-view-key)))
         (multisig-keys (map 'vector #'bytes->hex-string (geta info :multisig-keys)))
         (signers (map 'vector #'bytes->hex-string (geta info :signers))))
    (is (= 2 threshold))
    (is (= 2 total))
    (is (string-equal "367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900"
                      secret-spend-key))
    (is (string-equal "2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0"
                      public-spend-key))
    (is (string-equal "bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02"
                      secret-view-key))
    (is (string-equal "deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9"
                      public-view-key))
    (is (equalp #("367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900")
                multisig-keys))
    (is (equalp #("156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"
                  "ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8")
                signers)))
  (let* ((info (decode-multisig-seed "02000000020000005b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c95b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750cce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"))
         (threshold (geta info :threshold))
         (total (geta info :total))
         (secret-spend-key (bytes->hex-string (geta info :secret-spend-key)))
         (public-spend-key (bytes->hex-string (geta info :public-spend-key)))
         (secret-view-key (bytes->hex-string (geta info :secret-view-key)))
         (public-view-key (bytes->hex-string (geta info :public-view-key)))
         (multisig-keys (map 'vector #'bytes->hex-string (geta info :multisig-keys)))
         (signers (map 'vector #'bytes->hex-string (geta info :signers))))
    (is (= 2 threshold))
    (is (= 2 total))
    (is (string-equal "5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c"
                      secret-spend-key))
    (is (string-equal "2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0"
                      public-spend-key))
    (is (string-equal "bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02"
                      secret-view-key))
    (is (string-equal "deba7c0440e2e7f7da4fa77470e98f339089d3fe0c609209b4a388266451d1c9"
                      public-view-key))
    (is (equalp #("5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c")
                multisig-keys))
    (is (equalp #("ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"
                  "156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced")
                signers))))
