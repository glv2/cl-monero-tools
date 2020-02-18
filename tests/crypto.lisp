;;;; This file is part of monero-tools
;;;; Copyright 2016-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite crypto-tests
  :description "Unit tests for crypto functions."
  :in monero-tools-tests)

(in-suite crypto-tests)

(test fast-hash
  (flet ((fast-hash/hex (data)
           (bytes->hex-string (fast-hash (hex-string->bytes data)))))
    (is (string-equal "7591f4d8ff9d86ea44873e89a5fb6f380f4410be6206030010567ac9d0d4b0e1"
                      (fast-hash/hex "01009091e4aa05ff5fe4801727ed0c1b8b339e1a0054d75568fec6ba9c4346e88b10d59edbf6858b2b00008a63b2865b65b84d28bb31feb057b16a21e2eda4bf6cc6377e3310af04debe4a01")))
    (is (string-equal "5ff8734db3f9977eee9cf5e2cf725c57af09926490c55abd9d00a42e91a8c344"
                      (fast-hash/hex "36f9f0a65f2ca498d739b944d6eff3da5ebba57e7d9c41598a2b0e4380f3cf4b479ec2348d015ffe6256273511154afcf3b4b4bf09d6c4744fdd0f62d75079d440706b05")))
    (is (string-equal "eead6dbfc7340a56caedc044696a168870549a6a7f6f56961e84a54bd9970b8a"
                      (fast-hash/hex "cc")))
    (is (string-equal "a8eaceda4d47b3281a795ad9e1ea2122b407baf9aabcb9e18b5717b7873537d2"
                      (fast-hash/hex "41fb")))
    (is (string-equal "e620d8f2982b24fedaaa3baa9b46c3f9ce204ee356666553ecb35e15c3ff9bf9"
                      (fast-hash/hex "4a4f202484512526")))
    (is (string-equal "d61708bdb3211a9aab28d4df01dfa4b29ed40285844d841042257e97488617b0"
                      (fast-hash/hex "eaeed5cdffd89dece455f1")))
    (is (string-equal "a6d5444cb7aa61f5106cdedb39d5e1dd7d608f102798d7e818ac87289123a1db"
                      (fast-hash/hex "75683dcb556140c522543bb6e9098b21a21e")))
    (is (string-equal "aeef4b4da420834ffced26db291248fb2d01e765e2b0564057f8e6c2030ac37f"
                      (fast-hash/hex "0f8b2d8fcfd9d68cffc17ccfb117709b53d26462a3f346fb7c79b85e")))
    (is (string-equal "9a0c1d50a59dbf657f6713c795ed14e1f23b4eaa137c5540aacdb0a7e32c29fc"
                      (fast-hash/hex "512a6d292e67ecb2fe486bfe92660953a75484ff4c4f2eca2b0af0edcdd4339c6b2ee4e542")))
    (is (string-equal "81147cba0647eee78c4784874c0557621a138ca781fb6f5dcd0d9c609af56f35"
                      (fast-hash/hex "ec0f99711016c6a2a07ad80d16427506ce6f441059fd269442baaa28c6ca037b22eeac49d5d894c0bf66219f2c08e9d0e8ab21de52")))))

(test slow-hash
  (flet ((slow-hash/hex (data)
           (bytes->hex-string (slow-hash (hex-string->bytes data)))))
    (is (string-equal "a70a96f64a266f0f59e4f67c4a92f24fe8237c1349f377fd2720c9e1f2970400"
                      (slow-hash/hex "01009091e4aa05ff5fe4801727ed0c1b8b339e1a0054d75568fec6ba9c4346e88b10d59edbf6858b2b00008a63b2865b65b84d28bb31feb057b16a21e2eda4bf6cc6377e3310af04debe4a01")))
    (is (string-equal "2f8e3df40bd11f9ac90c743ca8e32bb391da4fb98612aa3b6cdc639ee00b31f5"
                      (slow-hash/hex "6465206f6d6e69627573206475626974616e64756d")))
    (is (string-equal "722fa8ccd594d40e4a41f3822734304c8d5eff7e1b528408e2229da38ba553c4"
                      (slow-hash/hex "6162756e64616e732063617574656c61206e6f6e206e6f636574")))
    (is (string-equal "bbec2cacf69866a8e740380fe7b818fc78f8571221742d729d9d02d7f8989b87"
                      (slow-hash/hex "63617665617420656d70746f72")))
    (is (string-equal "b1257de4efc5ce28c6b40ceb1c6c8f812a64634eb3e81c5220bee9b2b76a6f05"
                      (slow-hash/hex "6578206e6968696c6f206e6968696c20666974")))
    (let ((*slow-hash-variant* :cryptonight-variant-1))
      (is (string-equal "b5a7f63abb94d07d1a6445c36c07c7e8327fe61b1647e391b4c7edae5de57a3d"
                        (slow-hash/hex "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000")))
      (is (string-equal "80563c40ed46575a9e44820d93ee095e2851aa22483fd67837118c6cd951ba61"
                        (slow-hash/hex "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")))
      (is (string-equal "5bb40c5880cef2f739bdb6aaaf16161eaae55530e7b10d7ea996b751a299e949"
                        (slow-hash/hex "8519e039172b0d70e5ca7b3383d6b3167315a422747b73f019cf9528f0fde341fd0f2a63030ba6450525cf6de31837669af6f1df8131faf50aaab8d3a7405589")))
      (is (string-equal "613e638505ba1fd05f428d5c9f8e08f8165614342dac419adc6a47dce257eb3e"
                        (slow-hash/hex "37a636d7dafdf259b7287eddca2f58099e98619d2f99bdb8969d7b14498102cc065201c8be90bd777323f449848b215d2977c92c4c1c2da36ab46b2e389689ed97c18fec08cd3b03235c5e4c62a37ad88c7b67932495a71090e85dd4020a9300")))
      (is (string-equal "ed082e49dbd5bbe34a3726a0d1dad981146062b39d36d62c71eb1ed8ab49459b"
                        (slow-hash/hex "38274c97c45a172cfc97679870422e3a1ab0784960c60514d816271415c306ee3a3ed1a77e31f6a885c3cb"))))
    (let ((*slow-hash-variant* :cryptonight-variant-2))
      (is (string-equal "353fdc068fd47b03c04b9431e005e00b68c2168a3cc7335c8b9b308156591a4f"
                        (slow-hash/hex "5468697320697320612074657374205468697320697320612074657374205468697320697320612074657374")))
      (is (string-equal "72f134fc50880c330fe65a2cb7896d59b2e708a0221c6a9da3f69b3a702d8682"
                        (slow-hash/hex "4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73656374657475722061646970697363696e67")))
      (is (string-equal "410919660ec540fc49d8695ff01f974226a2a28dbbac82949c12f541b9a62d2f"
                        (slow-hash/hex "656c69742c2073656420646f20656975736d6f642074656d706f7220696e6369646964756e74207574206c61626f7265")))
      (is (string-equal "4472fecfeb371e8b7942ce0378c0ba5e6d0c6361b669c587807365c787ae652d"
                        (slow-hash/hex "657420646f6c6f7265206d61676e6120616c697175612e20557420656e696d206164206d696e696d2076656e69616d2c")))
      (is (string-equal "577568395203f1f1225f2982b637f7d5e61b47a0f546ba16d46020b471b74076"
                        (slow-hash/hex "71756973206e6f737472756420657865726369746174696f6e20756c6c616d636f206c61626f726973206e697369")))
      (is (string-equal "f6fd7efe95a5c6c4bb46d9b429e3faf65b1ce439e116742d42b928e61de52385"
                        (slow-hash/hex "757420616c697175697020657820656120636f6d6d6f646f20636f6e7365717561742e20447569732061757465")))
      (is (string-equal "422f8cfe8060cf6c3d9fd66f68e3c9977adb683aea2788029308bbe9bc50d728"
                        (slow-hash/hex "697275726520646f6c6f7220696e20726570726568656e646572697420696e20766f6c7570746174652076656c6974")))
      (is (string-equal "512e62c8c8c833cfbd9d361442cb00d63c0a3fd8964cfd2fedc17c7c25ec2d4b"
                        (slow-hash/hex "657373652063696c6c756d20646f6c6f726520657520667567696174206e756c6c612070617269617475722e")))
      (is (string-equal "12a794c1aa13d561c9c6111cee631ca9d0a321718d67d3416add9de1693ba41e"
                        (slow-hash/hex "4578636570746575722073696e74206f6363616563617420637570696461746174206e6f6e2070726f6964656e742c")))
      (is (string-equal "2659ff95fc74b6215c1dc741e85b7a9710101b30620212f80eb59c3c55993f9d"
                        (slow-hash/hex "73756e7420696e2063756c706120717569206f666669636961206465736572756e74206d6f6c6c697420616e696d20696420657374206c61626f72756d2e"))))
    (let ((*slow-hash-variant* :cryptonight-r))
      (let ((*slow-hash-height* 1806260))
        (is (string-equal "f759588ad57e758467295443a9bd71490abff8e9dad1b95b6bf2f5d0d78387bc"
                          (slow-hash/hex "5468697320697320612074657374205468697320697320612074657374205468697320697320612074657374"))))
      (let ((*slow-hash-height* 1806261))
        (is (string-equal "5bb833deca2bdd7252a9ccd7b4ce0b6a4854515794b56c207262f7a5b9bdb566"
                          (slow-hash/hex "4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73656374657475722061646970697363696e67"))))
      (let ((*slow-hash-height* 1806262))
        (is (string-equal "1ee6728da60fbd8d7d55b2b1ade487a3cf52a2c3ac6f520db12c27d8921f6cab"
                          (slow-hash/hex "656c69742c2073656420646f20656975736d6f642074656d706f7220696e6369646964756e74207574206c61626f7265"))))
      (let ((*slow-hash-height* 1806263))
        (is (string-equal "6969fe2ddfb758438d48049f302fc2108a4fcc93e37669170e6db4b0b9b4c4cb"
                          (slow-hash/hex "657420646f6c6f7265206d61676e6120616c697175612e20557420656e696d206164206d696e696d2076656e69616d2c"))))
      (let ((*slow-hash-height* 1806264))
        (is (string-equal "7f3048b4e90d0cbe7a57c0394f37338a01fae3adfdc0e5126d863a895eb04e02"
                          (slow-hash/hex "71756973206e6f737472756420657865726369746174696f6e20756c6c616d636f206c61626f726973206e697369"))))
      (let ((*slow-hash-height* 1806265))
        (is (string-equal "1d290443a4b542af04a82f6b2494a6ee7f20f2754c58e0849032483a56e8e2ef"
                          (slow-hash/hex "757420616c697175697020657820656120636f6d6d6f646f20636f6e7365717561742e20447569732061757465"))))
      (let ((*slow-hash-height* 1806266))
        (is (string-equal "c43cc6567436a86afbd6aa9eaa7c276e9806830334b614b2bee23cc76634f6fd"
                          (slow-hash/hex "697275726520646f6c6f7220696e20726570726568656e646572697420696e20766f6c7570746174652076656c6974"))))
      (let ((*slow-hash-height* 1806267))
        (is (string-equal "87be2479c0c4e8edfdfaa5603e93f4265b3f8224c1c5946feb424819d18990a4"
                          (slow-hash/hex "657373652063696c6c756d20646f6c6f726520657520667567696174206e756c6c612070617269617475722e"))))
      (let ((*slow-hash-height* 1806268))
        (is (string-equal "dd9d6a6d8e47465cceac0877ef889b93e7eba979557e3935d7f86dce11b070f3"
                          (slow-hash/hex "4578636570746575722073696e74206f6363616563617420637570696461746174206e6f6e2070726f6964656e742c"))))
      (let ((*slow-hash-height* 1806269))
        (is (string-equal "75c6f2ae49a20521de97285b431e717125847fb8935ed84a61e7f8d36a2c3d8e"
                          (slow-hash/hex "73756e7420696e2063756c706120717569206f666669636961206465736572756e74206d6f6c6c697420616e696d20696420657374206c61626f72756d2e")))))
    (let ((*slow-hash-variant* :randomx))
      (let ((*slow-hash-seed* (hex-string->bytes "154eb7a21cd32a597f985644187297607bad491650091620550877239c4178e1"))) ; Seed height 2031616
        ;; Block 2033333
        (is (string-equal "6d6999673d984343456db0c45d9397696f14fb0e4255effaed3d740500000000"
                          (slow-hash/hex "0c0cebd599f205091810d1db0cf05c946b5d4201dfb84d39bc490df08c0b3da1afcbcdc454826652190d3968d76c0e37abe6fe996d7725c619b0f0029a72e036d7e34180a294bdd353e02006")))
        ;; Block 2033334
        (is (string-equal "2fe153aa7f180d43e8a60b1f58333c8ac8e1f9e8ab58445149d96a0500000000"
                          (slow-hash/hex "0c0cb2d699f205d66a0d13b2c006ac8a455b793440802ebf9322a8770f4d1b203348d6ab6771e9ecb60100b5de0362c27369af70bc419fd11a1a09fe2b8a991075db2cd0b953c3cbc1e9e810")))
        ;; Block 2033335
        (is (string-equal "5001dadbb1e3f65a94cb395d49960f95be1cfa06dcee53b844aea90400000000"
                          (slow-hash/hex "0c0cdad799f205dd2a430a4a433a15e16f81369277269873f8934d906f5f09523409651a4d6a3d4187037a7c2c9a7c34108285a15dab3653b2470b2d11063b1635df6583b34158e79d9ce51f"))))
      (let ((*slow-hash-seed* (hex-string->bytes "2ad7b6b5509b48e823a0a3076eb6ed71d403cd717500a0e10719cdf3a7ae1165"))) ; Seed height 2033664
        ;; Block 2035250
        (is (string-equal "7842a365c54121eb2cf0f148de1f82b96e3ff8e71349cd61ca3e4a0600000000"
                          (slow-hash/hex "0c0cebe9a7f20593ab9ac755786a4982ac0641a3439ff888a8c197cddc011c1cc4ca3111f6b169c7e40700a8883e1da616e90f5d1087154c1531a50e08bc82023d37c670c32ffb86b64f6a08")))
        ;; Block 2035251
        (is (string-equal "fefed055d5d187ec7a5fc66b818828d96de19522c2a7ca72ddc3570500000000"
                          (slow-hash/hex "0c0cafeaa7f20582aae0b23f4e00e1cc9cf8fa165aa372540588967940271410f1ea2729184d8c62230400f84d3ad4d7747470da40be379d97a4179ac3276959c91ae6e2d387b182ff6d0c0a")))
        ;; Block 2035252
        (is (string-equal "99fc73be6d0c864a52779f3a00a257dca5148e9c44885ea83d47c60200000000"
                          (slow-hash/hex "0c0cb2eba7f205e7b123381798d2a8e96949dc474fd419e11e2b8fc1d04b7ad6c59717bafdc104d20206008bb8cbd49777a3b4a448893c16215d767a41f964b3f60888925ddb2e2de496480f"))))
      (let ((*slow-hash-seed* (hex-string->bytes "5a7f09e485cef6c38ed438c71352ec109d3a0abb429ffc799eb2772e51ae60d3"))) ; Seed height 2035712
        ;; Block 2035850
        (is (string-equal "a81d071b7edcd999ffb2f8a85e4db6c532fda82b81540c6d85bb880200000000"
                          (slow-hash/hex "0c0cbe84acf205f12c35c0b1b597192aa4e44d9507d61c1ffe3bebe0270c26dd23fcfe02b021dcebce0100cbbc6ec7fcbe51a2492b0194022410d82a736fe561aa4fd5ac29169193d15a8707")))
        ;; Block 2035851
        (is (string-equal "1f4aeadc0ae9663906f7bf947fa3d77c55f7b81a75de40a22838dc0400000000"
                          (slow-hash/hex "0c0cfa84acf20513d7f2a00b37b3755acb0bd9ccebddc2e39bec67f18e8f5ba3cff4ea7b88e2e44d580500b1ea5eedf4b2e78b428564780a38357cf32aa7b3681602566d543937f9d6594807")))
        ;; Block 2035851
        (is (string-equal "4d77288859efca3c83f9e345acf384caf722a42991423845dc0a710000000000"
                          (slow-hash/hex "0c0ca385acf2050399815d69b45b889d51ab72e616e220090438a6c5c52d4d0f4c16cf409db29b40ed088b482d11f3e4e8ae84af27ac01ac0c0a21aa01add4894e72fb4200e1b7d7fd5b7e04")))))))

(test tree-hash
  (flet ((tree-hash/hex (data)
           (bytes->hex-string (tree-hash (hex-string->bytes data) (/ (length data) 64)))))
    (is (string-equal "676567f8b1b470207c20d8efbaacfa64b2753301b46139562111636f36304bb8"
                      (tree-hash/hex "676567f8b1b470207c20d8efbaacfa64b2753301b46139562111636f36304bb8")))
    (is (string-equal "5077570fed2363a14fa978218185b914059e23517faf366f08a87cf3c47fd58e"
                      (tree-hash/hex "3124758667bc8e76e25403eee75a1044175d58fcd3b984e0745d0ab18f473984975ce54240407d80eedba2b395bcad5be99b5c920abc2423865e3066edd4847a")))
    (is (string-equal "f8e26aaa7c36523cea4c5202f2df159c62bf70d10670c96aed516dbfd5cb5227"
                      (tree-hash/hex "decc1e0aa505d7d5fbe8ed823d7f5da55307c4cc7008e306da82dbce492a0576dbcf0c26646d36b36a92408941f5f2539f7715bcb1e2b1309cedb86ae4211554f56f5e6b2fce16536e44c851d473d1f994793873996ba448dd59b3b4b922b183")))
    (is (string-equal "45f6e06fc0263e667caddd8fba84c9fb723a961a01a5b115f7cab7fe8f2c7e44"
                      (tree-hash/hex "53edbbf98d3fa50a85fd2d46c42502aafad3fea30bc25ba4f16ec8bf4a475c4d87da8ad3e5c90aae0b10a559a77a0985608eaa3cc3dd338239be52572c3bdf4ba403d27466991997b3cf4e8d238d002a1451ccc9c4790269d0f0085d9382d60fef37717f59726e4cc8787d5d2d75238ba9adb9627a8f4aeeec8d80465ed3f5fb")))
    (is (string-equal "e678fb87749ec082a9f92537716de8e19d8bd5bc4c4d832bd3fcfd42498dac83"
                      (tree-hash/hex "051a082e670c688e6a0fc2c8fd5b66b7a23cd380c7c49bd0cfffb0e80fb8c2334bb717c5e90db0ac353dfc0750c8b43a07edae0be99d6e820acc6da9f113123ae084c38ccdbf9c6730e228b5d98e7beb9843cfb523747cc32f09f2b16def67f76765cee044883827b9af31c179d3135b16c30f04453943d9676a59b907a6439658f6c98159b8fa1b152f1bcf748740754ca31c918501dbd577faf602c641df59")))
    (is (string-equal "7db3258ea536fef652eaaa9ccb158045770900b3c301d727bcb7e60f9831ae2c"
                      (tree-hash/hex "4231b54cddc617d06e0e311536fa400e5be0a35aab5fec9ec8d98f6c6dad3916fe6cdb1f63be231f95cdc83bb15b0d99d32d9922331b738c423625471fad7f408e60c0773fe78938b054e28b86ac06a194d141c1bde5f3c6f2b11468b43702cb3121b40ccbcb5461fa9321c35c9342e21efd7c1c22f523d78b9d4de28112b6cc51552642ffc126c66f25038f9d3b0cf485cc252215c144d51a139c8ea9a0ecc16e81d8d92dd3660d885deca60070d3d00069d89db1a85acb9c1f18d0c90736a7")))
    (is (string-equal "ad56b3e027d78a372adebe839e154668aec5236f7d40296cfdb562fca1dc73c2"
                      (tree-hash/hex "68e09573a758b75ea8e7d925fe81e3155afecddc4c8aeb3fe70d87411ee53aceac63c0233d172cd49b2708350fd64e2cf4dccb13352e3a159c06647c609429349197163eca2c2dae0c8643fdfe5d346b2ffd45a2d46f38599efbfa587c3ac0c3119e19508e009556fe53e4f78ef30eed649cdc1e090c8cb662eae1863fdc683bbabea966764f550a142dd68e5b8eb1930ff0c7333c9f2555712489a8cf6a5d188a70841510fca540b8c0425123efca47d5a698cf392e3bdbb7226053459fae01fd19ddb9d16d5f5499525feb49ffca9411e7ac48de15256559f3f65f899b80af")))
    (is (string-equal "090a95612ed9df6eeb854ae320355889a302498b4f5164a79d8e384a3a0d9748"
                      (tree-hash/hex "42e7f4058ca80d513c140837dd661acde3fb914779079baccfe188cbce275aed4b515094bb49ab9a825bcc2ac13f84b14a9defeb1b62fc68124b088272a3562696d62ccdfb5d896b2d2b410a2a79f9b1e7849feebc17617ba12a08d4e80affe970ff2fb79917ac13708f79be215bb6484d298b2fe22b4818536e74894db5e0350e1505ca2681da7b7d7171e3d10c89348cab160ff5b2e739d3591443d2af60db5eb36c50a2dfdb79b8ab83b0792161ac4756d9b831f1863188e10c81af5077d0fdb123f66e51670f03a203ff2287dea6827dcd5afd4904736ec4fe9f3b52f7e2bed7beaa1543bd8bfbfff6a8ae8bf1791dc34efa92c6342532fa33a3b72b6c9f")))
    (is (string-equal "997ac1178ab7414bab823fbca45b5630df8d1d8263063e6c57da463b85d68a74"
                      (tree-hash/hex "947fbbc55ad237fc5dbd7d52dddd44bf3f2a09005c78873422f7ef282d8e6fcc554e35c9566febf91cbbcb1d57a7ebd119abb0ad33a006d01623b7b379e966e00be000ae2fe8a45940e99c953d22014bae4932d8493ad4a551a97d437db2939dd53abedc11a63417f76257a5587f382a57d46d63c372182600c7920bcaf74e9e65289e8c45123ac8a54a45a6104dce5b8c065065ff3a3b6f8bf4d86bf96cb56116df4e01eb3153223d5f3a8c0d7de9eb348158e5ca0c363568674215f68b6ff8e54aeb4a2661f1144cb4f1bde7f9e6371d8a5568d4b3ff3382c65e143ae5d3a5834c890559be95b8b80b82c83d70df85c934bf9dd4b0f2b5f60b8553bd1c1e537b7a1f78a89a17a335a06f5d7143dfecff0c10a2e0a524c91ce913ae04501b65")))
    (is (string-equal "d7647e967e4f1ad3d5a0b2d231f62c4fe8fea85b845a72aaf57aeea96f2388f2"
                      (tree-hash/hex "5b0bf1b5c843cc5ea8e907c0d6ea901f1d4259cd61e68895fa1a9df76973ac6c87ee22343802565be146e4fcd768cde3cdd1b1996b8626e53b62648a9fd7f5aee2ce5b4aacb090d1beeaf42d47e7f0e90174af6554e8bd4aea3df45e90537eb7572b9583b3fcedf56ff69c412c4576a1353458292b7a6b10536887da47fb95c999ff1a074dfb52db43cf423e81e02aacb267b5f3b48761de9c3a73efe199d710e09043e4701792d04112d18e33d5f78efe4fbda461b4e0f2f55f07ca04eed04762d956b396ff0471c28f48462bf9b6b47caac50be8dd822198a39366071b18f4d4e8188bd11421b606108e9bbcfb1377e122c36083beca6a2306e48bfdbc64c9e6435ed838eba78e0af101abf79ff9600f6cc1b2b776783491161ae2d1d8df2d436a20c053c9237a7d224016878906352eba550d778e91ba830906b8d0be4e6e")))))

(test hash-to-scalar
  (flet ((hash-to-scalar/hex (data)
           (bytes->hex-string (monero-tools::hash-to-scalar (hex-string->bytes data)))))
    (is (string-equal "7d0b25809fc4032a81dd5b0f721a2b21f7f68157c834374f580876f5d91f7409"
                      (hash-to-scalar/hex "59d28aeade98016722948bf596af0b7deb5dd641f1aa2a906bd4e1")))
    (is (string-equal "b0955682b297dbcae4a5c1b6f21addb211d6180632b538472045b5d592c38109"
                      (hash-to-scalar/hex "60d9a4b96951481ab458")))
    (is (string-equal "7bb1a59783be93ada537801f31ef52b0d2ea135a084c47cbad9a7c6b0d2c990f"
                      (hash-to-scalar/hex "7d535b4896ddc350a5fdff")))
    (is (string-equal "709162ee2552c852ba62d406efd369d65851777152c9df4b61a2c4e19190c408"
                      (hash-to-scalar/hex "14b5ff33")))
    (is (string-equal "36ddbd71a4c19db5ea7022571a52f5a9abe33fc00aafd24b562fb75b7fc0360b"
                      (hash-to-scalar/hex "383b76f631652889a182f308b18ddc4e405ba9a9cba5c01b")))
    (is (string-equal "c381ea27500b61d29e9ad27add0168053cc1a5b7fc58b6960f67c147324acb03"
                      (hash-to-scalar/hex "3a170545e462830baf")))
    (is (string-equal "357f141395a76e2fd5003045b75f3216294eab0524eda1ed16cbe558145a2403"
                      (hash-to-scalar/hex "190757c55bc7")))
    (is (string-equal "b365e89545402d3e7d649987127980ec8339af2e3067ff942e305a9ac0b7390d"
                      (hash-to-scalar/hex "e1dec4027ccb5bf7d273163b316a86")))
    (is (string-equal "24f9167e1a3eaab18119c225577f0ecc7a488a309e54e2721cbaea62c3db3a06"
                      (hash-to-scalar/hex "0b6a0ae839214674e9b275aa1986c6352ec7ec6c4ae583ab5a62b947a9dee972")))
    (is (string-equal "8af86aa2f8739b7d384e8431bd1ec5a75a1e7d1dc67f2f7100aeffbaa516200e"
                      (hash-to-scalar/hex "232849cfbb61443dcb681b727cdf7a2b84116dfb74a3c1f935")))
    (is (string-equal "79b024435100e891c167abd8f96d3f5efc6919e5861f7298b7736f2927276809"
                      (hash-to-scalar/hex "0bd05745dceb00b2c18080e6cb66d9099e9610d620c188a9")))
    (is (string-equal "594cd0a2b135b1c29544b095b8a43e5b3cea1806fdcb9b59cc53829cc62f2000"
                      (hash-to-scalar/hex "ef2e5ce130838935ed202cd61453ecb860adbb903f0eb950df")))
    (is (string-equal "43ff71f4c9544c09e583d3fa4d21297463d029415e236ae758d06f4238b5ef04"
                      (hash-to-scalar/hex "48c7811fe63d09ceb4e6ad0acd51487496b7108d279078bb")))
    (is (string-equal "20a8a23806bfa8ac1e3d7a227bc4c3554a18f5e593e5f8b807767c3f818ebe06"
                      (hash-to-scalar/hex "854b5522f6a7a50af76e305c65bc65d2ad7603a00e244aabab4b0e419576c7b1")))
    (is (string-equal "995f4205c63106243983d2be160a2e17f2ac9b78c8e6a705a4c52d6adf2ada0b"
                      (hash-to-scalar/hex "3aca21fdffbb7305feed286925")))
    (is (string-equal "42138bd241761d92b67db8ef225347b98e10b74f6fb0123da7b44f8d51c37309"
                      (hash-to-scalar/hex "5cf74e22b8b6d30b90be7e2296f1e89cb76bd7ea3001663256")))))

(test hash-to-point
  (flet ((hash-to-point/hex (data)
           (bytes->hex-string (monero-tools::hash-to-point (hex-string->bytes data)))))
    (is (string-equal "52b3f38753b4e13b74624862e253072cf12f745d43fcfafbe8c217701a6e5875"
                      (hash-to-point/hex "da66e9ba613919dec28ef367a125bb310d6d83fb9052e71034164b6dc4f392d0")))
    (is (string-equal "f055ba2d0d9828ce2e203d9896bfda494d7830e7e3a27fa27d5eaa825a79a19c"
                      (hash-to-point/hex "a7fbdeeccb597c2d5fdaf2ea2e10cbfcd26b5740903e7f6d46bcbf9a90384fc6")))
    (is (string-equal "da3ceda9a2ef6316bf9272566e6dffd785ac71f57855c0202f422bbb86af4ec0"
                      (hash-to-point/hex "ed6e6579368caba2cc4851672972e949c0ee586fee4d6d6a9476d4a908f64070")))
    (is (string-equal "72d8720da66f797f55fbb7fa538af0b4a4f5930c8289c991472c37dc5ec16853"
                      (hash-to-point/hex "9ae78e5620f1c4e6b29d03da006869465b3b16dae87ab0a51f4e1b74bc8aa48b")))
    (is (string-equal "45914ba926a1a22c8146459c7f050a51ef5f560f5b74bae436b93a379866e6b8"
                      (hash-to-point/hex "ab49eb4834d24db7f479753217b763f70604ecb79ed37e6c788528720f424e5b")))
    (is (string-equal "eac991dcbba39cb3bd166906ab48e2c3c3f4cd289a05e1c188486d348ede7c2e"
                      (hash-to-point/hex "5b79158ef2341180b8327b976efddbf364620b7e88d2e0707fa56f3b902c34b3")))
    (is (string-equal "a6bedc5ffcc867d0c13a88a03360c8c83a9e4ddf339851bd3768c53a124378ec"
                      (hash-to-point/hex "f21daa7896c81d3a7a2e9df721035d3c3902fe546c9d739d0c334ed894fb1d21")))
    (is (string-equal "1a442546a35860a4ab697a36b158ded8e001bbfe20aef1c63e2840e87485c613"
                      (hash-to-point/hex "3dae79aaca1abe6aecea7b0d38646c6b013d40053c7cdde2bed094497d925d2b")))
    (is (string-equal "b252922ab64e32968735b8ade861445aa8dc02b763bd249bff121d10829f7c52"
                      (hash-to-point/hex "3d219463a55c24ac6f55706a6e46ade3fcd1edc87bade7b967129372036aca63")))
    (is (string-equal "ae072a43f78a0f29dc9822ae5e70865bbd151236a6d7fe4ae3e8f8961e19b0e5"
                      (hash-to-point/hex "bc5db69aced2b3197398eaf7cf60fd782379874b5ca27cb21bd23692c3c885cc")))
    (is (string-equal "6a99dbfa8ead6228910498cc3ff3fb18cb8627c5735e4b8657da846c16d2dcad"
                      (hash-to-point/hex "98a6ed760b225976f8ada0579540e35da643089656695b5d0b8c7265a37e2342")))
    (is (string-equal "8aa518d091928668f3ca40e71e14b2698f6cae097b8120d7f6ae9afba8fd3d60"
                      (hash-to-point/hex "e9cdc9fd9425a4a2389a5d60f76a2d839f0afbf66330f079a88fe23d73eae930")))
    (is (string-equal "b07433f8df39da2453a1e13fd413123a158feae602d822b724d42ef6c8e443bf"
                      (hash-to-point/hex "a50c026c0af2f9f9884c2e9b8464724ac83bef546fec2c86b7de0880980d24fb")))
    (is (string-equal "9d6454ff69779ce978ea5fb3be88576dc8feaedf151e93b70065f92505f2e800"
                      (hash-to-point/hex "bf180e20d160fa23ccfa6993febe22b920160efc5a9614245f1a3a360076e87a")))
    (is (string-equal "0523b22e7f220c939b604a15780abc5816709b91b81d9ee1541d44bd2586bbd8"
                      (hash-to-point/hex "b2b64dfeb1d58c6afbf5a56d8c0c42012175ebb4b7df30f26a67b66be8c34614")))
    (is (string-equal "daa5fa72e70c4d3af407b8f2f3364708029b2d4863bbdde54bd67bd08db0fcad"
                      (hash-to-point/hex "463fc877f4279740020d10652c950f088ebdebeae34aa7a366c92c9c8773f63a")))))

(test secret-key->public-key
  (flet ((secret-key->public-key/hex (secret-key)
           (bytes->hex-string (secret-key->public-key (hex-string->bytes secret-key)))))
    (is (string-equal "d764c19d6c14280315d81eb8f2fc777582941047918f52f8dcef8225e9c92c52"
                      (secret-key->public-key/hex "b2f420097cd63cdbdf834d090b1e604f08acf0af5a3827d0887863aaa4cc4406")))
    (is (string-equal "bcb483f075d37658b854d4b9968fafae976e5532ca99879479c85ef5da1deead"
                      (secret-key->public-key/hex "f264699c939208870fecebc013b773b793dd18ea39dbe1cb712a19a692fdb000")))
    (is (string-equal "1dec6cc63ff1984ee46a70a46687877a87fcc1e790562da73b33b1a8fd8cad37"
                      (secret-key->public-key/hex "bd65eb76171bb9b9542a6e06b9503c09fd4a9290fe51828ed766e5aeb742dc02")))
    (is (string-equal "25255c26721758456545bcaea3a99407cd3df7c8208eeb49bd80450627138fab"
                      (secret-key->public-key/hex "37621ebd8de6ca022419fd083066285da76ada9bae6d2b7c1a3847d78a726b0b")))
    (is (string-equal "1be4c7b195156a06ebf6a81acff06c3cbfcc4f869a8f0994e0d98d45f586b02a"
                      (secret-key->public-key/hex "a44f5cc6e6583394ec1970bc16e9e3b70c09ffb2ebdd515c8f5e6a8c02b5ce04")))
    (is (string-equal "577a7b3e1b89089936fa89729855e25ad646057309c21da272b38dd3db5da9bf"
                      (secret-key->public-key/hex "46ca522f94c1ce7a3755a158234f87872792ca03cfbb0aebc6897bf376d4a009")))
    (is (string-equal "f888cdc2cc39194e638587116ad14554a42b52ed13fbdc3410ad888408b6b7d1"
                      (secret-key->public-key/hex "93f6713bdcfb18984dccef06dd84690ffc054b6eb6f4c75ebdfe9bb0ec1f810e")))
    (is (string-equal "345c0c9279ac9686ed7b73648bc3e92e34d513e0d87d390d74250830976757d1"
                      (secret-key->public-key/hex "e49fa0345748fecd9b1f7a60b3cf2e6d61b47a15a033a390cb0586b7185a7f03")))
    (is (string-equal "043f8109c1c406aebfe8581b9d0bb41159a957a91b4d6b08ca18bd7a804bcdaa"
                      (secret-key->public-key/hex "195917df120eac02087610a7fcae94d61538e9dbbe6773ea45cc7cf3808bbb03")))
    (is (string-equal "cc690e16d540108096d73bfc458ed3695a60043fc048877920efc03860014314"
                      (secret-key->public-key/hex "0f588b029cf8d9b6efccb2fcda1ce33c16c552c11e5286807a74f6f7c7eb1603")))
    (is (string-equal "294352ec050ad768e14d2e13f6c588b8499171080106724edfda9b5e3d0b9e21"
                      (secret-key->public-key/hex "2ab2964c462dfb7abdda7229c91c1b1d217d015b9b429b76e93b7af854ef2200")))
    (is (string-equal "a6422d15a124f5e8c0bf9d8269d36df75de108003b5cc984be6863774c0b0021"
                      (secret-key->public-key/hex "11f4389bb16bcfb02de71b385345eb7593121097cac0a14387090f1a7dbe100f")))
    (is (string-equal "0dad7138095b22905f7074c4750fcdcd7ab4fc210ccc41812af50c9b26888afc"
                      (secret-key->public-key/hex "27a92a4c8ad3de40e5e51c89c20dff6086fff954f574494154ff4f18faf82a0a")))
    (is (string-equal "1869ced31a176913f66d77fcb776f1ae51d404004d0efcb18676ef6d9f18764e"
                      (secret-key->public-key/hex "27926691b86d9bf9bfb575f1325e418b46de258495185a3bc681efb9ee671904")))
    (is (string-equal "6d145f7b2454f6d38677cb1650d980a933dea237cb820bd330edcf9111265379"
                      (secret-key->public-key/hex "d5f7e654389be8829aeb51c4ed8a02474d1f72d0ed62484e9eeb403f0a9ae007")))
    (is (string-equal "59f5445dfe7ac2a13f93088caedccaf78b69a3460b70547fa249f2fb7f3dc5e3"
                      (secret-key->public-key/hex "be82dba0af095ecbd0cda2a1eb29a99ee8db1f30280164bfbb9c2375a884a308")))))

(test derive-key
  (flet ((derive-key/hex (public-key secret-key)
           (bytes->hex-string (derive-key (hex-string->bytes public-key)
                                          (hex-string->bytes secret-key)))))
    (is (string-equal "4e0bd2c41325a1b89a9f7413d4d05e0a5a4936f241dccc3c7d0c539ffe00ef67"
                      (derive-key/hex "fdfd97d2ea9f1c25df773ff2c973d885653a3ee643157eb0ae2b6dd98f0b6984" "eb2bd1cf0c5e074f9dbf38ebbc99c316f54e21803048c687a3bb359f7a713b02")))
    (is (string-equal "72903ec8f9919dfcec6efb5535490527b573b3d77f9890386d373c02bf368934"
                      (derive-key/hex "1ebf8c3c296bb91708b09d9a8e0639ccfd72556976419c7dc7e6dfd7599218b9" "e49f363fd5c8fc1f8645983647ca33d7ec9db2d255d94cd538a3cc83153c5f04")))
    (is (string-equal "9dcac9c9e87dd96a4115d84d587218d8bf165a0527153b1c306e562fe39a46ab"
                      (derive-key/hex "3e3047a633b1f84250ae11b5c8e8825a3df4729f6cbe4713b887db62f268187d" "6df324e24178d91c640b75ab1c6905f8e6bb275bc2c2a5d9b9ecf446765a5a05")))
    (is (string-equal "f5bb6522dea0c40229928766fb7019ac4be3022469c8d825ae965b8af3d3c517"
                      (derive-key/hex "ba7b73dfa3185875538871e425a4ec8d5f16cac09db14cefd5510568a66eff3e" "c9b52fd93365c57220178996d97cc979c752d56a8199568dd2c882486f7f1d0a")))
    (is (string-equal "bcdc1f0c4b6cc6bc1847728630c3060dd1982d51bb06873f53a4a13998510cc1"
                      (derive-key/hex "45f6f692d8dc545deff096b048e94ee25acd7bf67fb49f7d83107f9969b9bc67" "4451358855fb52b2199db97b33b6d7d47ac2b4067ecdf5ed20bb32162543270a")))
    (is (string-equal "7498d5bf0b69e08653f6d420a17f866dd2bd490ab43074f46065cb501fe7e2d8"
                      (derive-key/hex "71329cf72de45f5b98fdd233707501f87aa4130db40b3570527801d5d24e2be5" "b8bc1ee2987bb7451e90c6e7885ce5f6d2f4ae12e5e724ab8432769af66a2307")))
    (is (string-equal "796b938b108654542a27155a760853101aa896eba019c659e0bf357db225603f"
                      (derive-key/hex "748c56d5104fb888c4143dd3ae13e578100cd87f4af1be562ee8401d2eec81ad" "659f545d8661711e337ce3c4e47770c9f55c25b0c087a3a794403febd3f1600d")))
    (is (string-equal "6e9e6dba0861417979f668755c66e09cba4b06d07eca5bcadf6e8dd2f704eef4"
                      (derive-key/hex "1a2c6c3f4c305b93e6c09604f108d46c988e16bb78a58bbc95da5e148e9ca856" "668b766d1a3b09fc41a7f27ca50a1ffce1f6456b9d3613527f0cb86e1eed6705")))
    (is (string-equal "f59b6f915e270452eccdf7172f1cf0fe702beca9067673ea3ef7a4920066a1cc"
                      (derive-key/hex "6ac060a711ce299a7ee47a74f7b3ab9d53ed8bb19fe3bf5f786745babf22e3c1" "00b5bbef9ad292f0289126a0ece082c9c535324c5ee0fd1534f7801777337f05")))
    (is (string-equal "e4ced0d7c6d10f0dd4f55a4d7b69ad17b692179b0038013dc8ac287fd4360cf2"
                      (derive-key/hex "aa1a5a28ec965d1f4838c2781628cafa9867dda2153990c7fc4d19dbf1cae3b2" "76c1838f52d761c3738500f240b14e48ada3c1e92081d5f60e53d642fffc610b")))))

(test valid-signature-p
  (flet ((valid-signature-p/hex (data public-key signature)
           (valid-signature-p (hex-string->bytes data)
                              (hex-string->bytes public-key)
                              (hex-string->bytes signature))))
    (is-true (valid-signature-p/hex "57fd3427123988a99aae02ce20312b61a88a39692f3462769947467c6e4c3961"
                                    "a5e61831eb296ad2b18e4b4b00ec0ff160e30b2834f8d1eda4f28d9656a2ec75"
                                    "cd89c4cbb1697ebc641e77fdcd843ff9b2feaf37cfeee078045ef1bb8f0efe0bb5fd0131fbc314121d9c19e046aea55140165441941906a757e574b8b775c008"))
    (is-false (valid-signature-p/hex "92c1259cddde43602eeac1ab825dc12ffc915c9cfe57abcca04c8405df338359"
                                     "9fa6c7fd338517c7d45b3693fbc91d4a28cd8cc226c4217f3e2694ae89a6f3dc"
                                     "b027582f0d05bacb3ebe4e5f12a8a9d65e987cc1e99b759dca3fee84289efa5124ad37550b985ed4f2db0ab6f44d2ebbc195a7123fd39441d3a57e0f70ecf608"))
    (is-true (valid-signature-p/hex "f8628174b471912e7b51aceecd9373d22824065cee93ff899968819213d338c3"
                                    "8a7d608934a96ae5f1f141f8aa45a2f0ba5819ad668b22d6a12ad6e366bbc467"
                                    "d7e827fbc168a81b401be58c919b7bcf2d7934fe10da6082970a1eb9d98ca609c660855ae5617aeed466c5fd832daa405ee83aef69f0c2661bfa7edf91ca6201"))
    (is-false (valid-signature-p/hex "ec9deeaca9ce8f248337213e1411276b9c41e8d4369fc60981b0385653c0f170"
                                     "df7f028022cb1b960f2bd740d13c9e44d25c344e57f8978459ffa3c384cd541c"
                                     "2c2c8e7c83b662b58e561871f4de4287576946f4e26545ba40e78354c6d0b36f69ea44892f39a46cf3fd5c2813cbc1c525dac199ada6fd5ca8e1e04cff947700"))
    (is-false (valid-signature-p/hex "114e8fffb137c2ce87dd59eff7f4b8e6cc167fdd28c3ea77d345d2c8c00989a1"
                                     "d257f46216be34be5589e0b12094e643d1b31bc3c50e006d044d1ea885b5007d"
                                     "9579b6e8dc108633ac8b67004699921aef479b6e7ee9590073fbe1404ee4b3d533dec29fd35540f13ac531c3ae49abb62cbc11d36b0cc3353db77a294d8d3d92"))
    (is-true (valid-signature-p/hex "ce03e1fa5476167c3ebce1a400ca1d2d375176b5cb9ed180913efa1a688ddc97"
                                    "a05a3a6776f85c5d04c42fa2c6a731831c3d3a4e3a12f967f9ba0b1ecd1aee98"
                                    "4992de4fec265113710ec3a211e86784581f96241f0305d069a1e4629b504d03b3a1561fd9e73597db89ba00beeb60d2107c1f835176949bd354e8a173d46705"))
    (is-false (valid-signature-p/hex "7db838c96a3e1fb14156986aef37b70f932ee79d3cbc8233cdd76997eaa0c0c2"
                                     "306593abefdbe99beec4752ebb135131a93e8361fc35f60a1c56fc4501c6782f"
                                     "5bd47b285d25ede033bc5c2049edf3feb06fe29091e2c90ba25128c6c1a050713f28db1b9106013d22d5e0ba05bbaca43c4d30b6f0bbad8768e6cb89b205c20c"))
    (is-false (valid-signature-p/hex "2d96536cad13a409d5a46a6bde1f5cf1d9255e741d5a17309248dd910e02d1c3"
                                     "c2257e5386cdef44b989ce395532b8e03dde166ba26c18759e1c440738242fe4"
                                     "2f5e7a5c690c0d3bb2974e47eaa159d0bb2205a636b8cd09736add9fe8d75bee4249b30f8e1b99c1dea45999842f5709d2ee1d8e450807319723625074c69605"))
    (is-true (valid-signature-p/hex "40e0758cd9c9f8f8f7d0fbf351084863973a3622c92dab501ffdee610278f86a"
                                    "f79812b95048683d47eb5435bdd97c5a39532c6693dc8b965af76d7f3ab88e92"
                                    "a9ba1cb8bf2898e21c12bfd23788994fe20d45ef6f775c197ab157d7c2721100f2123c19395f13ff79941e4fc9ac33b2f70077a79c552b4ebc97a4321ae66e09"))))

(test generate-signature
  (dotimes (i 10)
    (let* ((secret-key (generate-secret-key))
           (public-key (secret-key->public-key secret-key))
           (data (ironclad:random-data 150)))
      (is-true (valid-signature-p data public-key (generate-signature data secret-key))))))

(test valid-ring-signature-p
  (flet ((valid-ring-signature-p/hex (data public-keys key-image signature)
           (valid-ring-signature-p (hex-string->bytes data)
                                   (map 'vector #'hex-string->bytes public-keys)
                                   (hex-string->bytes key-image)
                                   (hex-string->bytes signature))))
    (is-true (valid-ring-signature-p/hex "c70652ca5f06255dc529bc0924491754f5fad28552f4c9cd7e396f1582cecdca"
                                         #("9cc7f48f7a41d634397102d46b71dd46e6accd6465b903cb83e1c2cd0c41744e")
                                         "89d2e649616ccdf1680e0a3f316dcbd59f0c7f20eba96e86500aa68f123f9ecd"
                                         "3e292a748b8814564f4f393b6c4bd2eaaface741b37fd7ac39c06ab41f1b700db548462601351a1226e8247fea67df6f49ea8f7d952a66b9ec9456a99ce7b90b"))
    (is-false (valid-ring-signature-p/hex "3da5300a7aca651dc3a85016824b0620a19973eae4af8910cc177faee499358d"
                                          #("7af6983daaecec1bff70b05c7369fe1636270f8dd606d39eba974b8c1d5f6091"
                                            "78c2676d12505b7d5a63ce29f736124b48a02dc78bd0ccd1e9901344811bbd3d"
                                            "957251408e9a8b255adcabc52bf15cb8a05501e2892d7cdda22ce672adc0cd12"
                                            "73e433a1668d056d9b651aead47658476cf23c39cfeb4fd23625ed94af439677"
                                            "f58dd3a46e0a07000ef6e978cff87604c32cd3df487acd220b53b5ccc46d0bd5"
                                            "3480f2437e01073133ee9cad9651665277b09bc0a46618f975a746500f9f34bf"
                                            "0ed796ebd217fcae58272ed7c4c0b058f558961f95a68ea52f59e4e6f0374d73"
                                            "729e5938a034b2f50b583d4ada5541a12de09aa1776653a821da7d6c6b057716"
                                            "1a9855ca5dc19cf11f49a87b182695451e83952f2df4ee9009bf0f72d6e25194"
                                            "c609386fb270c7af8dc4ee102fd33ed3c836e7ea493dc79655c9feb1cfb9c869"
                                            "94ce9f9d3fbe4848aeb59231d749c6c9f7bfaaf99c31e4317c2b5b1b335f20f9"
                                            "9331dabe4d6f230a7c45417dbbb28b0808b4b6bdcba83b774583d784413ddb4b"
                                            "437652a047fe0872b264c094440625cb5cca5cb8d10cda950138c97dd09e5943"
                                            "5c470fa7fc2684336917a626e2dd4a09d68b4ef9499857c11e8ef0aff77a9262"
                                            "df3ccf869b0262bd33952ace2b1320f809e227949ce6a2e89a245ce5da75e250"
                                            "4ec480b685a07d091057e73954c8bd5e4646b2bd0bb0a46a38f74cc44e0c940b")
                                          "8d39d8d877d74cccbd5fad872b8297eec3f4b3f5187486f8c98a2ff27f994800"
                                          "4e7ee4fb45708e2bd97b2ee4e134224714c2fe1c0b679b7de838d715e281f10e4da8a6455bf4683a91e6e56be119fde36ccd61f35aa4867e9725dda7c18bb40afa097778cdcec51fe2d205db00ab9a00dd5f6f0a2a3b8393a7ae2bc03d6ff5073b25f72be8166bdf2e21a4745841bbff68a54d6fa3c77577a6b5c0f0b7ce0a120d07d704e9a30f306a63d5b137534ac4b60953abeb420d526f4b501008bd0a01790d806d86cf96391953e8b630833a3d6a01f023da720e26015287dee5c9b9060f43fa3574626a3673628b6b82c4b0251b5251173b59177fdc8f79974205090da7a3d93d1d274ed39654c348870da21ac66ee7a0e072e504aa01ece4051c3f0c93eb6065ae0e4d05e49144b986b33ee741c406860d39c953062531f2e993140c43aeeef6b144cc42bd13c2aafb10402edddebdfe0b426730163fd530f7b2c3009d4b98d761131c5ccbdcb737b4a1e9fc72c1313fb45eef13ce4d31c40ec94c0466cfcba1771229b0d2e014d716c9a2c10f6191eb67c1bb05e72f291d1d98290c2b4542d196df41410992428e36c62965a2d68f1fb99d6d4058b7e0360dae2c0c56d7980a546f547d2944c3bb6672fdb7d40378d47b0bcd9ca9ce55abce22ee0d94a7906ff1e31d37578a919dd92b4615ee0930fb33cb494548e13b731b33320193bc5b88e5864fe2d5d9a1b2bfb343177ff10d647337205f22a37b5cb385090bd2a050d055cfb9fbe01cc88ffa3fad76eb5b7baa50cf5f379ac6cf2636da8308c945c859cae02e772e432b177238a6e3d75b5cc6e676d1f3989d78493d86440cbb5552586755101dc4dd43bf51cab188fcc919012f7d9b19db5189fad02362059c8b3cacc8f80413e9a43f41cbe154ad55acac98287edef98240b6d8a04fcd0a020c8481164041bd7165a6b90b8560593c8848d29995634392dd22f3e993bd05580e41349091cb125eb2048c53dd230808019482f4b22c83ca678d9b01931b03f84ff3b4a78c138fbe36d013c9a423e0233962be5f278b535b20f05c829624043641b8386f7cd01a77c4077a5a6da0eec912042c60d9b6bcb56cac9713dc7002ab69ee15e0013204f68d6093b158c21f3a09e205b15e627b3fa33d9a0cd0300d9287b5d75ab7dd914ed95c8ed3db0bf05f68792be1492c41854a2e36c123d30427ff0fdf706d5a01f6cf6b1c02e2140f7b165b2259904528f12b527a7de76a0d8f90c3c027bb2f71cb1b345fafee0628b3f15049cb577b780890b9b96c53160b9ec99cea512304c94325b2c894b6dbc926c9b2b8f497a765a180f70425174802033d83980e2b28cfaf767d6a4da871d6adee474fb093e89df7eab67255b1b70821e64bee166892d1e5ea9ff2dae7b65231427e01c63164b5f3f84626511d2f077de2741fb75e3d940646480b595338c6043cdda23f0fb48d875498675e686eaf"))
    (is-true (valid-ring-signature-p/hex "90660b84dd3be5705c7766695fec404348af6df58f8c5d58213f3b70b8b67a23"
                                         #("4af96f2c3a70ac1860d48132136989c1d38551367025d43f36aec0ffa8e7f28a"
                                           "376cc178d8ae3a68ce467bfbe719e88b22514617dbd1e764e0b94b4f6bc961af")
                                         "6289b9b151eeb263fc29e4b5e90978db7670f06f408403c8973bbfff2a884dd9"
                                         "4ccadd504d1d03e385ebd25dc51b98c6f3a0e1c1be7e5694e44dc2377898510ca3202d7872294cc04b65d8c109e3a6e843c327b3416ca3a2b1c585fe4152260555441dd7b1543549f749acf5fc9a93a3f3c240425c5f7cadccdef4f06cef0702ae4ad477d0cb60a1a48c1da22f5a8b20c7c5672833c7ae13f78edeb3db1a7b01"))
    (is-false (valid-ring-signature-p/hex "d280b24c280daade9d2bcd68c6dfd39d3a13eb1b0645c4f7d2b0613dd4b5af3d"
                                          #("2d4e494897c24b1730f018df65468c2647b2dc19f650d1a9e055b9319045ff13"
                                            "74db9c16b0cb4beb7d48ec77b654c63917529072aa57d381b5e3b8dbb06e0f5b")
                                          "f1b943daa1ef225726215f551dfd85f56a3b429ded8608a09a8310a90b8aa88a"
                                          "8aae0a8523d65b3746c87994e4cffaf437ac147a82efe34389d270a976183006c7de37ef0362e13aab9287a85445748a8e0e1a357c6a0ba090f436937a1878b47b41de38a3737152453ca3c0c6546b65ceaff3298329273b0808d35af376a20c1217c85b153d40bc154108eca199175b3efa3f190740325c734d82cfb054d50f"))
    (is-true (valid-ring-signature-p/hex "17e1d8c991803cf0747a66dd16a3c5069afb0f604670b823b675bed5de59d6c5"
                                         #("130f844d2ff629d6374653997afec462eceed08648daff08eac4c58b9006e6e4"
                                           "f92f7aa2bb9273830b966f71c7d7aa0ee8473973d65fa044c74ec4d4628d765c"
                                           "8c1f5b3b71c27ebdfadefed2594ba57b19934eda6fb7b5c7e63dd0ed471b6e2e")
                                         "81abb2291ae3e208665370f4fe07c1d82d3f8f6a6ccafe7e5fb4819ce1d2f113"
                                         "7e799950f135343936af6719ebcedfe6e4a3fceaa86047923f592e1fb69aa909575174936ecf6615813c0a4620aa77161d8309aefffd6d33b8eb31b37aa36109dceafe0b8b49a5a280561b204f71f1c6116053ed1bac94b26fcad0ec947a9b01e4459a956e4644f7a8c39719164a87c93d21971366e66e0409556fc93c4c1d0f7db9b2d221fdf6fae05cca363b5e9ea1a7c9b0c80080b9c825f9bcc0b734030711b981b71f0c193bdf51b41bdca81579144e1d7ea134b93a6ba40bd18bb74f07"))
    (is-true (valid-ring-signature-p/hex "f97d4e9c36389eb32dca83903d86e738b412d0e719fac9de9cb1a79deb5c5f87"
                                         #("e360cc0b0d54a051adc6eee4446c516d586d5d3abb484c792e15b69c1376a392"
                                           "b8a788bf7dfcd8f6cab88b156b04dea2bc337c90ff783f75327a7c32368b94c7"
                                           "4ccd4f2d7aee244994139bebf641fada943bce4c7a5fe96b7f1691a033c97b3b"
                                           "024d0d3dcec8858639fcc7d1d711d64d0ecff4c51e86b9f00a31f74b558593fd"
                                           "990bcb241da35a4edd7b67a9595c66053cb6f56a18b28eda8fba2ab1a0c40b3d"
                                           "628450cc2fbbf75ff8a63269bb004089b46b9041ef45f290813a3ead125d3f5f"
                                           "aadbb083008b069c45bd0838cf9dd55597982deada11f1a6f0bbb12db334b984"
                                           "f7e4485473d386ef3f6beec68c0c31ab7902186b4bee28012013307604a14ed5")
                                         "a710922602dd9d7891df50d78c8df1dd244ed7258d43384378212a09623f61eb"
                                         "89e0b6e11d9ed3595834d4448516173fb9bfa69bc350442e82f2ff158c72660dfc4f03ce65c088d5d3a988a9d6cf98d04559e99479f2daa4deb9d156a71e120c109472c1d0f0cdf12df970073acda0db505d4708cbc0a974c83312b1e58fbb0318efdb085296eb51ea81de0bdbe0535c864c83851eabd0df9789e75ac0179a04f9de1ffd04f2f4e3b205750858cf7ed6a5694f90a3d7259582e9fb0e644850054ad2bee7f7c75cfbd32d3bf7f6377437d99f818bf844bcac079579583e5f4401d5c0b79b94bf582b2fd0cb9e4739b74d71f235e1d8041361e53ad85e357a620a0df80e51d66476af588468dcabbd399f5a33d003f62dbc34c73cafceefb64c0d301ae7937538a1817e33030c11133029ae10e05b4d89bdc22e65aa73b29ee4004542792c62c62b9faf8c0ad356378b989356c45f8af36d98efde96ebb731b2099058f165a4c15c40182a4b87fe14849d19e9f87ab1bd9f1ec8a24166dda85203ef7e3be56475f20ef58f9a67e9e0ead0f07ac6199825ac7768e4a52add86970dafbb5b8a98ecc0192b074b6da1912a85bd26cd2990fa126a460be05a37187b0e6fe5f20b3466ad71233cda314137910beb7f4a8f92889a1f42b0cf53421cb502d8eeafe6cfdd5c02a9568a45a1db2e1db53451839e39d09dca7426354b3f0904bf941341584735e926e8fe9a139322c2f6bb82bdba106f9e2825ce09a2281407"))
    (is-false (valid-ring-signature-p/hex "4b3685c13ee27fa3c7c4949b799a6d61cf3fcf3dcc0b0b5d163bc523bb58f53e"
                                         #("4471da1870dd4b1d670887c52da935b786155ba1f33e4f285f13573c5ef85b5d"
                                           "bc140365276347b7ddb74f1d9709a1dc706b8c4784a8df0614718279c0d5fd5d")
                                         "60e6f33abf3013caf974260f3150460adb67772965a165ea25bbba1ad20f9ffe"
                                         "66caaccdeee7e254a5894090ba8295023384d4c34f65233609829f7dffe83805e18bf2641d51d564d53f5a5604e9f0486b234f2465e35ceae867d57cd9885908887ca45cb91c0cb1f3c061200fc1cfbd576ae5c47265a118880dc96ff44614035ff3c62ed6d37853b081f1b397c2f1ad74e96f87fefd502398b7e1deac4dbe0b"))
    (is-false (valid-ring-signature-p/hex "91d5d69145328db2b0456e9137859f9e2ada43794858a0f65060b31186b1733c"
                                          #("d228faef88768e5a51eb5bf1f70bdc1f37abf92fe02dd3b767236a26f6d93f5b"
                                            "ee5caf80b4dcec7d7227ceb6d77fd7e692e19953700e983b671dfc9d38705246"
                                            "3d1286dc5ac990769b7d2ae70d65836fdb19b7927b89c4fc19bb180ab5c4d263"
                                            "bcc29ce010c0a61810fd7ffcae604749577b079b64326f16d86b0f51603c9ff6"
                                            "11150530c271b48dacc42f2ac180e3838005536a5d8d35e9e613fea9c26d625e"
                                            "31b189ca5b0faed14d9d44afd6332371cf39c5787ce63bcea9b2a758859a9d94"
                                            "0f3f6e35b20a11cf37459b579c859fa1e35b92b56f04d1a9b3ee1a0836e51740"
                                            "eb9cc00929d7ff7617631b5f10a15024be3a1e8e3892dfe7e71ef41f391b87fa"
                                            "867509166d2dea7620a036da6430d16b92fb6e1ec1f23a6114595e5a57897976"
                                            "f543b10cd9558098efb7cb077177e311beb27537305139e5e9beb50fad24bd25"
                                            "e8fdce69689a8691abf8c2349e91976217289b573c7af1c848f92ed46013fccc") 
                                          "68beeb9ca45266b16dddb8056bdd44a76716dccbc97ef89dc47cdca8c2350cb3"
                                          "3d3f22cf0859853f3954c6f39cdcd0d7338f2baab1a72b32a533ca2d7da50606a2ffe2d032751b2e9f1dd09d165d608895fc9712edfc7fdf49c8c9c0a62b8b083a10098222594efd0760dc5b8ab4761cc4fd32946278c43715f126baed8acc069cbfb6874cb61000b194ea30046c274ecfe7347f9d9f79c8d33f3fb0d5d1cd0fe055a9a0161e676cf18df3ac5722e2984111255926f87e3a768fc237476d5a0fa2161f836223672a098e469f2b37904de7f229fe4d24b30118cb99978a6f3900ebc14a898506224f43394ad166bd692859f015b27286c5fc56705aa8a4d25549eeecf5ee8467bd4287dd6ec922434dc2c87ed567ef9b5e8367db9ab0da1f8602285237ae23be9082ab92d32a41efc6994f228148dbc78537dc97953a6e51e90cda0336f4beff1c166b8a4b9b424e90ecacac60589a466d83d2cd6dd24d8ba4052b9c860df7d53a3cff605b8c21f4bcad637d02e5402fa6d0fcfd23fdcc87029ae17ed35f861a1df56e3c12bf9d48fc80044d09d010c774be0067adf315e53a076e759732fe77eaf82b305185cadcdc01f185be5dac89c5a4e0df908ffe3e2d0a3faa8bef5dfc9c81db165a95e6f98e76ba38a7aaf3a1c4e3445b00b6ba157f09067fe2adfb45328bdcc44c2d77bee5c41760fc8fe14b0ad5d68cc252467df20dfd7dc1825f5aee65f60b082d8391a25ae04511e9a2136856fbe2b863021d1a081c6e1e47f799c9a5195f69264739ac1907f45c4623c49f8b749a105b60ea230f338af872e1928af743297d56177ade12ff67e07ce294f80d13798387b42b07c25c091c580828074dc851be60fddb098fc461cc34ef79cbd57436600d5c58b104556ca5ec3991621dc6dbd79abd387a5c5d75fda3346596599fa1a6cea26c4c09827ff05c304efece79ddb2a2f194cdbddabd54cfb8dea050c72a0dd19af0ed09e99d3069666f4ee33b4a7bbc0c311b9e5d285a5b65d974e38256ededb602ba00"))
    (is-true (valid-ring-signature-p/hex "8905840ba2829e2bb6b7f834db939b7ff006140df120170253e73d6a110adf09"
                                         #("7f68e1932829141a4823849121662b5ac7c8c50f0a3ded4359896648875bda84"
                                           "c0bd288abfda7dd2f55ba0580fd0b240f7c202c889bc9de914c759802d05c984"
                                           "aa0dffcb5ab16eae256860de6a14fb6cf2c474b828cab2f25fdfb704fb247517")
                                         "978803e051ac24bfc730d0dd1f746754dce0cf527357883ab71536802e4100c9"
                                         "741e3d899a92965d92bf4dab092f647138e28b4ac06ab729333795872b26b10cfb5bcb8ed397f5f76b185c6d8a541c3a72bb925b3ab797c3f55ccdcd14215c039d0ce5c9284433be61c16a44c5b4555eac61265b346bff52e926b9c7b2f2e40e776b60386568258430d9f4e2034d4e0eb5dbc737058e8665d1d9fe123a06680c6d113440b33b70a0eeb4a625ecacf30b2de1aa86795298aed80108d0a27ff602d689d89ab73d78c4e7a4c2c744a7dc25fc82372e76f1a4df9b34d68425b10708"))
    (is-false (valid-ring-signature-p/hex "f9d51e98ec2e48852e71852dd03092e403ae335d6e712c7ce7a135bda5a41833"
                                          #("8271049ee2bb50fb308eb6a0042205494742c4ef105b3b9aa0870f8b334e32cf"
                                            "5c68f45750e9290b069b8860cfa4d0f08f6812989c8f9fa1802f57bedf67c59c"
                                            "cbde7ee9a3b0fd58d04dfa282e9bcba785bdb1dc60ab660676faced58f691189"
                                            "b0b7ccf22728c75f2a5238eb9b677cfc87a34cbaa203ea1845ffefc518b9f449"
                                            "670b179d3efed230f867f3caa7bdb61b4ab8d0cb9ef89d99ade0f58aadc69093")
                                          "856dcd0cedbd4f363b4d8ef3a32588cbd204f2206681f7980d096505a5be2296"
                                          "364ef7f698541b4baf8c939806ca83aec12f1f27ace2d3050da0895fa0574a0ad43c98167ac7a36901f2a2f214b0026e317207068f45de05cc4ea67fbf2b060475c1cd6d24be47b99c4f3050d84488b84064b8d307994a0361453e7b88450c0b7b8131f8a2ee22b9d41d490a45e814b86c10a4ef0b074d5dd0b5df522db111190322c6fa14f31b43577ad9d2dcd29c2d5aee632846b14522a4f722ed4647410f1c59bd0788f12ed2ca64670144c2d2ba3dbd6164a8e71eb68bb5b7a1bcbb22090936524629c736ab57bbae68db652fa719d9d45dd20218e193fbe6bdac195d0d2a73260e2e306cfce06aa220d41677c25da6cc47c21d41efe6cad282169e830d902e636613123ef44fa9c4539808a507d867fec3bc063f2fbf8f66ddeea67c07eeebc52a03ec566050f634a8f6f543b94b357bc2054b04f819c31fa16804c400"))
    (is-true (valid-ring-signature-p/hex "a582c951daed8d5934300f4f62dca8cbf4afced14df907723bb998ac90ec12c6"
                                         #("7e777ef0e3d120dd0b333d5535ccd084feff7256857ed69bc5d4a38cb8ef967e"
                                           "7b922b179dbf6c782f5b5ba3deff76f3fed7923be210261fca1315f32eadcfdf"
                                           "6be26c7910b76f1f489b1de8c6c35d33c68cb05e718a87ba6c23f583cefd0223"
                                           "1c01742ccd62789422afd1236a81a1f0bf23fecae14e525a94b97bc4d337c35e"
                                           "58364d0fc67af86879e113189d561386cfeb7d2703698e265f2a72e81c0960d6"
                                           "114e1b802189d26234849e711ae83de1482c9957a1d96da438571da544550837"
                                           "06ca49298a78979be29eea3112e61a7103abb873e31d594b10f2a6a0e86f2962"
                                           "27bdd7e9a8525369473f7226a29c8609ade4548613f4d12567958dc9449245da"
                                           "dfab8a97aac47a260e833c97c99784f105eca2c2b266471588b2caf258e49da1"
                                           "29cd7df8c9f0bf34c0f0d9920498a83fa5b34c9fa5a96391ebbe554a62e2aeb4"
                                           "49dc207555e9acc438336583f3c6c671cd30e955b5d03b84274f95fadb5522e2"
                                           "bbd8d1ae66dd2034330428727878e1b720c8f57ccb33453262c8a55a4554f111"
                                           "66b8f8d8cdc368ad72529a73ef6f6c2ccd48e10e91d78fb411cf6ea2c703211e"
                                           "05c05274fc5d0777a32b94dfe697458f97a29040f712514e25d0541007f301e2"
                                           "f0bbf84dbf3162a783d5bedfaf590d00a4d521a3af7a4fcd9b62acf803aa8951"
                                           "ac6b7380a45753ce46dc4959d8138cc66e5ba88030b8e140650d34fe5cc93b87"
                                           "6208e49329a1d34b7a83e30a5329d989c7a52b382d7ae056ca119a108815bc36"
                                           "f824724e0b7dc12c2c0841fce07a1ecd9c77b0bd8f84c7c9250312db037632de"
                                           "a37293bd8717467542394c42c4b7f200922fd513963c71f62bbf97f63286fd67"
                                           "8a670ca75ac7ca3695a99953fd7e685959b1319e5b17fc3fcf53461ac3e5ae18"
                                           "ee5d487644b9da61ef2e07b111c4028caca3ff4993adfde12540cb0c265bfc60"
                                           "4c53c2913b5e3311968e9b7d147e473022173739c7d2a283f4977b358150a9d7"
                                           "ce60d435e1422499aa9c207ee1cc048af47f1c605c257acb93276341b3ecb310"
                                           "1b71c1d9d02fc8e77469d335d7e04c9b06d477fb0d10b3b0713767356a7f93be"
                                           "2b3fdb85c9ad1028faf36e1e8f82a58ae85a0da04bd4f82a3ef8a035dd83c402"
                                           "43b67114e107da5f5199250067cd9c3e6d2c03ac32ef9f10b94b50b4d89144e7"
                                           "2e65c6a2613b3fe85a12812dac5af331d67470a9ee7254ab63404f30cc735eb9"
                                           "d6699c569cad4cd4f24270477a4722c48c6f7216b6163eac31d6305f7067afb5"
                                           "5a20cac4b501f0813d94c2f90332d45ad4bc89f20ab3825e9b694d26458288a4"
                                           "466602c8797f666114621662a6e56c5094bf7f572bc2efc2f0cfa1515299b7cf"
                                           "73ee2bccca9e8aef60c3759902d8989cf26d47bf4a1f02e78dfa8cbf7719cf23"
                                           "758e7da50e467a485644fa130220ac21ce21206e92abc52b5de4b7b76632633f"
                                           "3a2485a9d6928c4b776b5dfb776e4052089314c794b9629bf5308e8cafdb4257"
                                           "1cbb272cb8f42c6ebfb5ef2cc2cedf29a942a1ab62c2bcf9e04ffe68e23352e8"
                                           "9df54116d6e9da454067a67bd8b596950a25236816a11ed8e3f93d6ef35bd4a2"
                                           "74217360442ac2659fa041595e4fa94436fc39eec0f51a8206569c4860d9ff3b"
                                           "e12ace7aa27e1eadcc982a916c31d004c871353e7dd6a1960997d67c9f7df6ee"
                                           "8c4961aad8ce7626cab6cb536eca3a34570313672674e858dddcf1431ed6526a"
                                           "cabeec41334e621a0647bca137e679996f58d79a6b9f21e7b2fe91ebba7c214f"
                                           "950ea0ed8ca85d2a215bcc940611e74bb8ff45b9a308813d7ccb2ee1c7173702"
                                           "dfff2636aa7ac1d8bedfbe5bfef16c81bc56e71c62bbb0f67a0d4a305c4f2364"
                                           "eb09350043d2693c5710ee8e89469c4a852932b8bc42b6cf812e116c561f176e"
                                           "86801e1ba4f84fdd4f2a37016494e6083079bad1a035919b25645667a8d814ea"
                                           "64ec08f43f4c387fb79e7e2e29d0e20364446db52f24e54b610adeb303b65008"
                                           "4f561a8b4655c5ebf1a24d838457f904e705198850e8b7078a042d4089c032ea"
                                           "9203bd689f05c6b86862be12c5118a689ac4cb97a20b9a47c4534ef8eb8daea1"
                                           "438387551c640b3e93bf2cea525f09c4bdff7232f7aeb8c0d1c8d16b34d1816b"
                                           "a5c0b15550e9d145a7bb651e12f9a6c3d2f7d6ee4ebf649446553b195409e118"
                                           "a8dc6ced103efbc01c54b7c49f50d4317cdbe52f42cc3284bf5f8349ec30f255"
                                           "42428a0d55adc561f0afae568e714581f9150131a26be30f14ca04693cf5cac6"
                                           "b8de18764096df84c8abc3362fdf35d8e1cddb1565535dc9c4129d791d7669f1"
                                           "5a48b75815ecb885fb1e4b26a5424f054b504bd95d04112e9d492993119a2f73"
                                           "5808e7ec9ed1d720f4e0132ad69a3995489a4a5c04d9d6a9d9b2805b43db0575"
                                           "14fa1dabf1b8ee746a30d935babcd4a0d0973320c762f95259b3904a978540d7"
                                           "fceb83981e37d5b8632c248c2aaadf256e8c8e779f6af332943580e5d22cab6b"
                                           "b0e8902ea99f6e207cbe709cc373179693361af152085f52e15dba0d928dabce"
                                           "e3194b10a9ee8fd345bae01e1dbf69a3816acdf95d0ee6401f34540c6c1ee304"
                                           "f89ed083f70a1a71df076746ae63b1b654586c08309578e42bf7db58238b88ed"
                                           "a167ece1891a8888710f783127d8459681ef2b128707a8eca673fc53bdd3f621"
                                           "427e97c7bda0632258cb6d04887226396fd36adf4addc850eb3c31644cd4e2fa"
                                           "bdd318ae0c70b010bb3d9075295f29b46a2734237cedb5fdb814ad9826645a6f"
                                           "4b92c0194000f161831cbfa1a358a5fbc209c9d247e554c4feab5d8947f7c359"
                                           "062e62b52ac8e820e85c66691cc5ea4c5d389b110ede32ea28e438d1cc0b298c"
                                           "49ab49baeeb383bff654b14105b7076dcca3c9328b4686d5ac9482d624fe361e"
                                           "0066eae5047d3a56f8d55028ddd2f560cf8ef1df739c8542d33a57be057848d5"
                                           "06b726f64a494d169b38d1b3b80604058bbe2ed88ba22f85845bb01ae6b6d7d9"
                                           "4ac0793f6fea64c54a1d8c1e1b76887d248dc8e87006a449c0e2a42f7836ce93"
                                           "e007476790e068ce71b954767e6c33af1ee388673df91c7e450964d3ed1ee5ab"
                                           "872c3a9ab8d192ddaaeaade4211dc93b14eabfa59d3a7fc7c8cf99e1b2709293"
                                           "c6b19b41b19a45c9424bb665e1def554052367378925083e7a5e666093fab893"
                                           "7b7bd3121d8a1284dc8d260c4344698af9d90fd5ad93c3012801457547038443"
                                           "ef9753c4715cda8fdfff8f703a6a7d339562fc78a496bbc15a1686576bb2a7c3"
                                           "23eefde0e4701468bc5a0588d30dd20ef1feafd2181441731bde5d62dfc1bdda"
                                           "ddabc1039fda5d3acba6127e2de56d863011ef403e88f6c9efaf4d7e5c1d7bc4"
                                           "c786f77f9d06da2d020a9900f746716584c25ac75e73ea9d77437634c99fac64"
                                           "29facbfa218b579acb3f1c6157980369904dd25c8b9bba3f9b85655b707ce974"
                                           "a1d56bff2b43a3db39538dbfc0a0a030b9457f5ccad059a313ec873587a8e26b"
                                           "4746c8364862d918f7ed56fd24b449e3e58fc72363b41516fad0f7a603cfbc88"
                                           "73a9dcaba1a7fecbed9fef18d7ba61fa94cf133fde4a8c8fc9103bcae075d015"
                                           "a1070caa8c9a71b70f2aa2167641fee3d84b54465dfa613f7d31588ab92591d7"
                                           "cc28051cd8e0cb33fd34c55c41e2e6d58e12226c27ccbc7db8a4bb991ec7a148"
                                           "d6d2ddfd553bb4bc300574d1d622d47f9574562ad9f2b14debbc89540909b6c0"
                                           "13d941cdab477c153e535996ae0c95eaee640c903b57c4b3b018a867389b721d"
                                           "402458941512fc096591e76a957e897831ef55383ed8f61b13fae2725bac8ecc"
                                           "aa1df5777501b688a610c81359851c426de0adf15007923f00d6329b47789b34"
                                           "28ed5fd7b6d2c7de9d011e28694f6b0fed9310c5c258e3f7c7b3e1c0455b4d0b"
                                           "e9157b510bd88a4099af2fb74461d2930a1edd205216b78e3dd7008965a52bbe"
                                           "8813fd003213d80b30f9390b7662bc0fc7483a79eba148949bc2c412c52a4ae2"
                                           "1f10f5f2d4a207142eb48a4c1c06f93ae0a3ffd17a39839cd0a867c6e7c0513d"
                                           "1b18aeaaabb0853e60bf1c08b59b707c1d375c5216a6632b7995014550bebfa0"
                                           "c660572d346863357a6da32995a421b236bee08447a9dccf34a6d9f0033c83a6"
                                           "9254c83a3b926bbedc4a77c5aeed4ff6c395bc53c1cba52189b492883541778a"
                                           "e8e45c363933d25b28500a33fc3477d7034aa935e6deff9421242365d50336ba"
                                           "11036342c257b80f34ae01f74a0511f587de23a14ec7af5ffeaa0c05fbdf2b9f"
                                           "b215ff6f745771c6b5ebe783d7a59ccd3d9ef5fa49e6e5d7fd57116f7d67dbb8"
                                           "8738b2f086398a81cf9fa5f691c4f01757b4ea9706ae18102e14e842d193ff26"
                                           "b5b979f067ffb730ea01994d1fd368cafcc0745fba6f207d1cfa47cb1e31deb9"
                                           "e76150888cabf98de831b1360ae31f92dfe3e023d81d3416696e1a726b79140b"
                                           "4f0e961cf1b7532df4025b74fd9db339fa5a33bc74bddbef5a9612b44411a00d"
                                           "c4e55b8a2ec30acdad6f492866ed64aec035d2d64a4a1b575e21a521855ebfb0"
                                           "7b48df08a10e641ece5949f0375a3b7f2081e7c22931e2dfdd87f41690dae72a"
                                           "e3fdcf909780380b7e6327318d38690e91b4de031b05b73a998e6dde02a3cdc7"
                                           "8a33aa46e3728a78c5981d2cd1a2ac7dbd3c417073343c5a7d273ce119879e32"
                                           "263eef94470be426658d255544839620342824cce0c99931be680468cc8d18a4"
                                           "dd5866c6b98bee83fa4d06cd74100646ceb89255860b060322549638cd68b077"
                                           "cabfcce35916eb6b6ab178d7d4a2237d78a329a205b7cda732965bad88d32641"
                                           "0c28b6490f8c8a0f1447c9af700a6bc72f07f8258f61179325fe2a89827d52c1"
                                           "4317b07c326dd869790dd4dd6fcc4718f99b4c4c3d0f029f2cd70770ed050bc9"
                                           "769b9b3a5b3041f2af835963d1db78f5aee35184f502f3e3b5d879eac31cc4c2"
                                           "445107e823cf3999b83afd38a2cbb6e560583362d8bf7a05c7b48d1778d50239"
                                           "f0f66bd095b53ff80fab934b7f9499e13fcc1ed34292a0ce7a55dcdf42cc3a28"
                                           "81a51f110ad7dff181f0718298114982881cee0f58cd9451a582a9017b1e7216"
                                           "d4e7bc3d880fcb0371db432432562d828867df68ea9a91ddd22aa0152a9af45e"
                                           "e55136b9047cfd2bb63ce4a5dc0baff0a2776cacf12f7da2e9517e736a408e3b"
                                           "03f6e5607ccdb46639cf96af33a6e2ed08bc44114a74ed2c1a12b6654901cc7b"
                                           "7edf02acec618f2e35c57f0b948055084d19ed1608dd176dc7c11028e26da083"
                                           "1d6b6d835484f15e44bbfcc8e9c5889e0bd657e23d90803e084b40566943e9cc"
                                           "1eae0d0fadae3ae260093a9f7d20d3590370bea77c1a30d6792925e3b3e63eb2"
                                           "018b489de6774689d1b75e12a80694279867003175ea9b481704ee40a2443039"
                                           "a0bf7730c7465e230bfea2aa80f19d5279793fadbf88201291871a0bf57fba54"
                                           "3d73b9e5868fc16ca57d3b23567946bec36de5e735071f4d80c987ffbb249d67"
                                           "848218d42e5d95da5f1946d6a3f1dbb60762acbaa94da7a8b5304a6be59c4f6a"
                                           "586bfc8fe9396ebd08cadf8ed1a21a3ebe3db657bfb6310f82ae27c98a7ce0a0"
                                           "b3932c83bf4a1115b20b4e02f392bb5543f84062385c93939449ade29e7c0d01"
                                           "52ababe6202d262d9f0df1ffd29b88ff557902bf18a2da7da67dae2ba34e4720"
                                           "d4cddf9ba86e29ddeb4d39b5a54da08cd181a1a13e1ad171c860eacc9a2a14cc"
                                           "969c6362503b7873b2ac2c5177cb4690bb62a468011dc5363bc4699b641053c8"
                                           "43e0b337c1a50c89ef76544c638d20dcb623274dd4fb771f143ed83781202fa3"
                                           "23811ce42f3c2aab7801496fcda96e2e2ef10f07d82016ebc67e5a08f29a1f29"
                                           "c2bb7bb03758868d1af742d5746bab95364dae502fad8b2f81adb31e66bc046b"
                                           "4534db57eaff6904ed218743847e4272219e2ecd171c604961861ef448934d12"
                                           "036ac2469c261addd54287d94935e0c12194b304d204884f23fdde78896bebe1"
                                           "accb95e018f5030d4ce9ab2003565b0a99e93620f0a493ff28e7e637efd136a5"
                                           "205f1bc7c6081f33add44081d3beee0b40a06adf7b040202696cd51c74b1d89e"
                                           "17d18cf830b2ff3d5bc4cb0b94660020bdbbd307be28a88f9a234e10cdba8af8"
                                           "7ef4ea83c78665ee3fd15ffe8be1ce70d3131fe63c3cc9b6f455412139397fa3"
                                           "fb60752a4e870b898e7c1da122aff9fef8c806ac03d110bb7f20b4b423849005"
                                           "e0fcf678ee4bdbe854949eddd52930d45ad6db460025e92e609de4c7c65cb80f"
                                           "c42ea5c3097dd762915598dccd636fd00cd8326d174e9d76058aa8a169075d02"
                                           "05de71cd2659d352263648b4ed0e3bb3da1b6995c24436153c9f035dc0f53276"
                                           "ab055e5ba753271e3e0802100f0aa47aaeadfbd2d09fe6eafbe4a9b76861ab68"
                                           "a6b8d764bf652b856686bc41353e9d5d25b353dd7563268fdeaac88d5fc0729a"
                                           "22514bc3f06716ab2497d954f4f84cfd1d91ff02a3ea7af56d34ef32326a4ba9"
                                           "56def77b87f9c0be6fa79d52976625f060c6e3858b210fb72a82ed569d5e96b2"
                                           "c2e59b5e646385224a7e7642ee3cff28e3043ea25237920b4eaee43d159b377e"
                                           "5c97cca046ba9457ad7c1007b4fe050d155499cf7a8fec6cf12ea25aabda8f66"
                                           "dd94bc676e43d7b01ebfc5adbeba11fb34340b942b9e192f6171ddfe501b7e87"
                                           "6d1e995ebe50bd4d1f2cd9ab88e985311a0a6637a7aa7b390ddaa72ee3af02bd"
                                           "fd7da877ee9d36e1cef8f6ede184ba029e828ea9562a6adb4b277fd11482e26f"
                                           "a80eb4834232090f3dde34fea84780eb94009a4b9f0cbc179d074d377e51bf83"
                                           "aeed805fd70b1cb0a01e76494d96c07c038fd5a3632425707378582b810fa92f"
                                           "2c4d509ccd0d646aaaff34a3b5471a53687265e3703360a1c40d5fcf9fff0cf7"
                                           "6685e32dc4011c90f0c16e1c60d6d0fd761a372cb4caeaa0e4b9d6b8107082b8"
                                           "c0056c29ec6aa1d91bdfc6446e46c2c2191ced61aac796047f17b0ae8194a55b"
                                           "b40d597762e5c9ccd8cc034230abe7a65022f3bf447e02fe6ac5cb096d0861c9"
                                           "7aea51bdf38d72c5cd982e5d360f4f360b965336d3641b2fc2d46b08526a6526"
                                           "d42c2c15abcb811e69ef2b5ccd4841fb22dc7e9e30b2ed5a6c473c887e54c9c2"
                                           "886abdb44416c9d6d01901886f639028138eecc3f492d6e684e38c9d6fbaf795"
                                           "486c66114fdd7e688c5796c7b035eb8e5b46ada2d9645fbb6400d5bea63b4202"
                                           "3041100d25e70ab6317a23ebb1a5877486300b4f0c850c0d06b01f7f351887ec"
                                           "41ded15ba5471bda06972bef8b289ea41938df158b69f16dabe205c7f3c660ca"
                                           "348f7da4a13a8909ef8a0ebb34418bcf78c384a5032e0806b01b55b943809f7c"
                                           "75975ab8b8b041087f3978b876f57a4eab205939e6a5b97b2b5408deac941e2b"
                                           "6495eed19b590fe2413c107db6b9674d0f19062c6e50b66d40a4749d12395118"
                                           "618de5454c06accbbe39a2adf79dd2a220e544c6f1eaa072416c02ea129053d5"
                                           "5c70eb68affa529603574aad33c9aa2eaa177d4c2c0050c37b8fe37f8de57274"
                                           "60528440e957fff5b2df92f8f29541afdaa9b5f3be156d9e4196468ad83ea7c6"
                                           "78d27cb54d8484c38ee2414806aafc2817f3e0ac41bfe2f0bc656a307dedf3a0"
                                           "754ad4606300333ea68309a72af8e2bda16a5ce9a724afee3aa8c00e9f2bb253"
                                           "2f8c2cf6f78998b7ab6d622a75e7ef594da389d41fc3be026c75643ab0ea5ef5"
                                           "756a7b18cb85d7217b54ec63f21fde6524c610fdbf97381a39d28e67a3698867"
                                           "729b775c84c83458bb8edbe6a6b4850276ba6d2c773e1b8f50bc4849c45b8d32"
                                           "be8446cb0553866f692d9da970c1cf133fb985c636c5401415d2991836114faa"
                                           "55be5bb652d0206e534170610f24345c8024bb6249c70376b59ad1c540fb57e9"
                                           "44f1a4d77204ed76b1f52a4bb5b6701d74b060963508fff4e7bf77548c14e117"
                                           "178f6aefadca48daea6f324db7ee845ecff0ad5235bd52e2fad461608c54a5e6"
                                           "1417f0e49a9660e18720fb5728bd717156e6a0d01ddb4769b02023b2aa46cf21"
                                           "8451aae431dc72b5ec1663eece440e91f66395aa072e0320d4dc367c86b9bae4"
                                           "ae08120263907a46d076791ab4659fdbbe4ee78dd3b2dc13ebf58ac77b50f935"
                                           "1c3736be9370e2276b2dadb6a1d9e7475447e51d251a9fa4d9f562f9c6d06ec7"
                                           "eb07b2d04799ad0b2c5b67052a7adb6ec4b2727328430bd7af11b48729adeff9"
                                           "daa8431c0f322310f524fab93801eef5f555d2b3eaed468932b8e9d99b4c12fb"
                                           "a9502f0345859133e95ea5c1088f0ea30dc77a59fd6dd876302f0f4403cd2ee9"
                                           "881cce607c851c9c3af9be56ed5efeb810610190bd97a5e21cb29b2d99c35880"
                                           "3e756aea2a78b6db924cf264b34995d9814920c680d768d3a588f5729b3e066b"
                                           "6a064f166e48d3f5252eacea19c638f7c82d98f9cfb596fda236874b3c0b6ca3"
                                           "18b4870e1c41af197870b467bc6d9c3637f7023b6072f3eec91b3ccbd91e482a"
                                           "ab5a825f8b3aa69bde3935f6b368974bf589ba0e7169cf008e43e2fca9d4d079"
                                           "5045c147d2ce820ec92e3e0f242a8396b3ca87a7580980818380246f846a5d96"
                                           "698ddac885ebf3a6779c42497eca8b7311052d698847392a1727d14009eb76a2"
                                           "b67c19acf0c557909cc989a41cd7016e59bebb26dd69dc3dcfe14cb3f7758d33"
                                           "3db9bf055459e8f65c80600e1d55c2049c5cada1832fbf7d28df560d98aaf2ae"
                                           "6e9a328dd0d51cd41e7266024796d36b09c704df07c4e1a05a8afa77e725e6d3"
                                           "eb4c727c362dea12b823d7e083b892ef253a7d4a47e5f2a063d80af7bb626a57"
                                           "f935e42e8d15cbf966199c87518799db008d68f2cbdd776f71e1584e779764fb"
                                           "276c5a136f0d601cb23f673381cb94352fa3fd3f17d418ec8311803529d1f5c4"
                                           "c92fc337b78737465212db89ada40b0e3945bf72dc0ec6b7248392defb625440"
                                           "719d06422431c811cc05f5fe4a44d1e78b2fc35d49684820e046ff6525863121"
                                           "af051e0f78a3bf31b09844db1b015beca614903502a4dcd5cb182a77d719b72a"
                                           "f17096eb98eca84a9140777409608f668855c948feea38fbc7dd5b03494b6f9b"
                                           "c439e2c4f328eab3f16a2dbc8811806fdffd6f2a090203f2e4d6d69769c011f7"
                                           "5d91fe161986a644b9834029424fea942738c38f3367b71cf2dfadd5929908c0"
                                           "7f9c640d9ee198f29b878aa3ec222046cb4896175dd8adec8503dee054dabeb9"
                                           "61fbd061f15c4b1a9dddcdbfad6f23c5864dffd69cba15a767bcd76ec4344d8b"
                                           "2f6d03b695c63470dd198ecdc22e497c9aec4655cc4b56ab93290892729e095b"
                                           "5866bf73b926457785303ab0d238f2a2ac7d65960283a5420ad9aa481aa3aae8"
                                           "b5e5bf712c9c334d251d3f6e1d605cc77a27961a89782fdca114aa4ca4727990"
                                           "985c78db864ae7c352d1772e069e308fb1bb5c0ccd84c0912a78324d7ce68e12"
                                           "86f7ecaef27cee2ce9e751519bbf94e50d209cd51d1a5a43f2a0b61babcbb397"
                                           "9e7f2cd1a3179299196f89230660b215355c5a9e6de9f014b1c6c459f09ac66a"
                                           "9e29e28928b7637bba2bcc8dd474430a60c53543ab94ee0b7850322ed8e2121b"
                                           "661f1f9c5c33ba9090f960f0ccef9f9267eec73de53018f84cdd126cba1f3d4b"
                                           "e279e2c7ac9d7016356757b2a0e3bf3987d07d91e6f9838be6fbcb4bcbe57d69"
                                           "bf081d226416a70f885d3213d5c9b727c329aa6f28e3af1ac92136f56be78035"
                                           "c1e23dc3037d3b70fbcebe2adf18a73333fe7782a407059b1d91bd533bc12388"
                                           "7a47c8a805559795dfa2c73d5a87752335a1d456472e9133fea8f40f589c1ac3"
                                           "3211f0978a16a3b589163fc1787099f22f98f76fd5a75a8e331138ee64c6cbb8"
                                           "af0c2286de54908b55e2d6e2a8b3e6416875e97ede7d53eead15a4413891d8a4"
                                           "8d3d65bb52cf50413169a284d41f3aad96e019a3dd2397e9e00833ea6ff986b4"
                                           "70125ac9763d647c33be36d7215abcf3244ae1d1b5f17ebbbc8400ebccb381ac"
                                           "6c101272b368dd7328eb5f04a0f644dadbf2f92d2f972b508b286ae8a453fe28"
                                           "1cc7cb1f04fe18d893dcc632d62edcb69fdba849163efed71da6aea8f056359a"
                                           "8d04551c0e670766a80c552215426cf0c90332b2592f50b84b95048385b7afe5"
                                           "33d5d1c2ab7599da6acb7c0b998128fba233a9bb547859d61193105bb4726d71"
                                           "765980b50cdf49f16883649c1510687265372a1e03055a6f2fb4acfd157e65ac"
                                           "47c46f77f2e1253b1e5776aacf8373de6fe2ec8e62e96fd9cbaddc1bc5e5e5e5"
                                           "bf49253e87a9adc917ccb83071c81201d7b914f41f3ca53b6480de67d8151362"
                                           "fcaaee825f1c8c4c5e4015ae97947f599db355be8929bc9a144e2db453ee8205"
                                           "5f2baf493fd8893e09bb06dd07e58eef4d1642e945e4efeba2fdaef0409546b0"
                                           "b612d4bd57994892e2cd6aaf453e5180d85661a1ef9b178f7531b9b99c7424c4"
                                           "e7285ff2f86e42362dd946c635ed3f88bbfb6420e2a147f2926b457b13d16e57"
                                           "b8411d43b3d19b1662e160fc1d549f2f139556846903ff9e1d2e22c38aec7a6c"
                                           "25c2b81e7ef19e900467c3cdcc044bffd56b83900cb1629caf2fd74b6cc05ff0"
                                           "60ffebb8283a539cfbefdd3c721a4c9f34370617f98e31590be0438697a246e9"
                                           "b7ff6f8a03b15b02a1111e67ed2f8bd13970008ffa4fda4630bdddf504070309"
                                           "c9c433a869678c6adda26e7cadf5cbe7091a44c184aec389b8d33559f5c793dc"
                                           "e4cb03cd5fe163c8bee0f22bd93d4d604b965bb5125f0d637a2d531bf468b846"
                                           "492cf9787bb24ba079c69f179566a4dfef6fa7f6b063fd880a0b3fa50ce7d761"
                                           "47fd32b65edd4274db0f6a6eb324c99de51dbcea2d6960dae52d34b8592bc5ce"
                                           "2f58dd0050af8b9b9c3248f969ab013af43ac9070cffa1c77f80098cf73c2f9e"
                                           "9f06a9325889797f2ac3be70298adeb75534e165cb39a39627ee631dfe555e2a"
                                           "cbbedc42b4d56f8c4af5f2bd446e7053340c4995bec4d40f2799963ec062e348"
                                           "e04a87bd9d07c2b67b52db8908bd2ca3c9b062eaa8b11df80d2c5fe0f5f2d601"
                                           "7dc1f5338c63fdcca6a935a2da04a0f0baeb984d4539a1df81fbb79baa176378"
                                           "fdd920dc465388e08560ff174191b493ce97891b888f3a451ae59c9314db9dff"
                                           "8a25cf6513ede70bf4c5594d927e04bfb9c48c43f7a65c5c73ee9c34e664a294"
                                           "7b780a216daaa053e91d91c6a4ddc2c75d7a535314f1f07aa18561768994d0b2"
                                           "92a027b8b5a143dba20d5e0f5ffd43bba074dad028d268698631ffb9f09da66b"
                                           "dedeb8293a7099838bae80eabe5ecd081b016ad0d63849302eed21471bd315ce"
                                           "3e2f3bd5e3ef44685f2423379dec6510e9f6d14a9d6882039888501e0966c4bd"
                                           "0145be38e28b54ece8b6c00627e45131cca7abf3bfdbc58e6ca496975525ab09"
                                           "56a3babf870725788bd05aef02debd57132cc0445df84c8b450235e8ba1cd4ed")
                                         "238b52edd9a58b056c875871eb4d5091a03c63af2cf923eeef4042f649aa4f3c"
                                         "3bf7e08289beb5ca431d681f5add4339f1496d8b5ce341678e9d3e2cf2ca870f3f52db9ba296b727bee818a26f8ca53c428ffa6f88c9428d2c076178f6ab450821e9c9ef49726f7f4a4a676712c9de91196a4a736e2af91a96aa754999fdc70ac8364c985a47adc06611a6b4888e97b3c68d0bea637d113dcdad613d63ce9c03e69d3f9393cf92dd3d307498cfc5c64b94a69170ed3805aa8baa72a1017f070c438d3b029cec4e0276e8c211c564679b4a2b7206f2d721974930a97f9dba890620c8db7d1286d2813f43ba52ba935eb3cbc5a52a97058be999f774e6512f7c0e1b25f41c1510b398dc29d4ffdf4de7447ecf9e6ada006b660d56521c00f113024090bff8dcc1b05a297d8633c33f89a5fb3865e02874dd9e77ba3a7d138bfa00a2e4b8b3927d35dd11ac6da2a236d658c9ca7e1fd5cefc7cf90d064dc9d1b203e219c7408044ea37822ccc989c0da975a55706759bf8260fd5ea7af53ada460000aa739c3f2f01adddc1a578d35dcd941318b710f5e234e2ae371e890a9e55057ca00ae5b9125cae3e0018b166ccdba9451d619041a969d348694475a3c7ff0c38cc7da2bf7b3e31f289a2e52f74a5dfb8086bb526ba6162ac8a9e2bee6e3e0f7946ceb2891b97a8cb37b689e4688bae7c771c871b8d8d821f9e5c43c2d3bb0ee9b583ea104fda8342282de6266b18bf3f6ca71faa52007478e340c14fcd510c408f4fc9d6c1e43944329c171133b981a60c2b1090b29c801f4b8c810f6d1605fae14bfb14c2ec91c8351db71523a82e0c4579a445a733ac3a72e4490527f60d19227a636bd28dcc4b6712b229fbb78891f096153e5258551282df8ec4f0620a83e53a198c6e891153114f1632d8fce38efb50bc76b36723edd02c6f70b4ba0840b7fc01119c1a2a9e5ac58288e28e453b436f1464aedf252f77862389544e077b4035d152d4fa18e4885abc20c45d28b4e885c3bed5c3dde7e793906ad58c0eb93a5eff8a8fe986a4c865e1926010361b9173476dc6bab8945e9e234ac95e01e1a2c55478e7e9f33e922b83df27bebbd4337cfb3be4401fcd72378c37851c074cc9bf3ca235846289feae7f2fe1a89502da4f0bb705aff1d6c8d8fb32db390236762c5db51045ee163790f333c343ae549e70a8c91045d9647be88460d5af004672a61f7c22bfb30d5bbb43e057d77052ca31fa34712817f0df9a1f9e667d004edc2dfa03871b0a0140ced73f6c2c052cb869fd9db328a9bd75cde31761a80e55477686ce8457846e90400c206231eb9e1b2942ab22eb2091c4b9d21dc7b10a5fc6a413aa8a432e996ce701aa4526c976761a345a1bf3c73639022957ec1b09db641bd28bd400627deec3f565f3bf487c4cd54e16aa2b5f17418cfa99dfcd0fc3dd0821d54eb9944ab68efa95b310dc202b9af900770032ee4c93349caf4b02f236b92fe120e143ac6eef76fff358220b6555937658d5f1bff743105b26120d52deed857a395ce2e596a3b2cd9f43c8e965a0b8625af2e9214623b5a66efc03c29f25e448ae8789780167938b4bc9cbcaba49a84ea29411d5683ac14ba99900f63aeb550951e630e2a41fcc409ea123315eb665695a9cb6a670b61de2919c02545c77a89ea2eb3755fa3cf7bdea6b96d67af7b3ab40a260b49af4c9228dc20b1a36eca9bec55622cbc9d06a31cf06302d1e665411dee147c22198b89b826c0172c33672d438edad8762eb86a86a9d73779c3902f34e01aa9cd7fc09111af20227366dc44451ee5f9996da793192ddd322b890dfb4d6bdfb7c389d6705bd150bc1c563e22aa99bcb61d4f59cff7b1a327b61d18914fa08db04860bee9c13bf0438fb9d50d99dd3f71d3f4fc10395aabbb5caa57f4eca212b402986cda123e30c012b7896a92f2421a57eabb2e6234129f591334c7c05f18a12b9fdd5609b4b0618c310a1aa665428b3c4c6afc1a0de6540f29a3aa61fb2b80f5767a789cf560b0b0a258e949019b22dc00fe624dbf7c12b4c917dad3b69fa370c3fc4013e310b8f8208bce4b1d322a7df5230c2462cd31bc43ffed858fa8ca7e3d08a43f72400cf97f08babcf4e34b2c522843884c53b5550d929258aeb5619c58c8473791d0dae3e341454854595c2316b5db034af4ae8393784108aeec9191ee387b4e76f058b2810727eb5e2072eb26c48c5ddb84b36d5713d8cf8495a0120f459ea41c20e076d9af81dafe4385a7e710f3102f028baaf547fe290dbefb2fefc702cbf2e0596f2d91fea8c4f8a82443b819d5a18377ce9575752989b8aa97f68fd84731505713b3c536bc69ba66b09caf47154c2943a4f66ee97db570e69187567a92c100e21af95b689c837c8761f8d226374a8083edab5d05158b282debbc14bdc076a0f0377aaf6b2cada6af53582469797f1fa1ced4eed4ed444cf01aa2811ec3734030a70fcafe67d4aa893c8c0cb7e2443a220369ec955b18c0c52657b48a363d8048b8ada4f939b468e72943301d956d6ea4cf0db24bca3fb09ec5ee91a8f466f08f33704f17465f3885703726fd293d40780971ae40b68c11ee604f23d9bf86a042aa88b723a5ab778b38b327f00548bd1d4499a9c53338fc8f24a70dc75c85d0409b6207e6d0de4a1a65cc7f2a9636fc135e0ea02967a8e4e821e17619652c20b1910eb9e415cc38b0d1384688e46820fc038229c8dac006815b8d9d82c983b0f2746936700ae5f3ab1ff280639f63f933543b5c510d8f1b367824129f0d6db0c0c89e951311d6014a653b136b4fbe7cdf8bce311f257d310e7d9b8f841d1d20e2748621d1891b637d22c879c985982dac37c1a7f5584fa79489dfa42e430e8045fd43fba05220d47df03c38ec3544f341fbe9f736b2dc3bf610cf9223ce7140e463bea322e4a7ec887685ceca4536c72145aee7fe712b9e1a9425339bddfbf0c7ce15cc1d1a4696485306be99d34f8e62108725b35681809c201e011fa9d5401af7f0da6a7076c28b268359ff7b6368634e3c7ff9d471c2ca68e7c61675a77005292f61fbbabece4e4fd14284067fe8d2929ef6848b9a6672c44598a9f0e4f0862f9744202375b932909dbad831566f57682205f6274cb2d9ddb20cdf9a78e063ac24cab8502a238364527fe0f5c1076d7e06844026563d637863af1e10b03061b224a01275854a9a73a00eddd38f2ee8ee8aaaaf7f15818a281e9f6a21cce0a3d43abf31c8bb3e110b45dbbd232c161365e02cc22a234936d8cf9c373acfa014d6ea5e81e565fefa465df27fe95a78a88b9e3b79b8aabaaa902654594d89608b529366af7356231185c91cead2e68f1248f058b8d00dc48d99e16a77b3be8046efe0ddc393c6dcfffe17c0ed655bc0aadd86add8c1374fbb79cdd7375ff1700c990f2c2b38af14e2802c527da616fd9c111e4c1f902040f8420b91d9dc9220b2d1d6384fa1fcc2c9e61264830b88b0e90ded0c39a84bb7b51158b35c03dc10a67a9eed191d1dc35f3942a259b0e1bf168ef5e50107342785fd1b47a22ccf8037a88da003eb889b52a3ac90996a3a4b711cadf8ab13825703e8fc2ce70b2d70dff64a024fcf1c0a5bd64086765d047da519f48bbf4814bfbd3faafdae32cbd0f8befc44089fa6cf8165584f8e53403333b88d5d5741ede6bbddd4367bec729068390fedcf6705f9453a6b637a10598a7ff4a16485ecdba18c34069e7ceb845060b284e2563a22e9bc275f6cb50a7df2254f0b6153b6ea6382198f8819c4f42025bb625b2509035cac8af075a88a861a10af2d92fa451d267d8c13f178cdad6092ebbe42e4e50a2a72c35fdf8cdcdd342f6424d7e9cd3ed2f0e0093020737b300361e706fe3747fe8269ab346005636f6646b6352962a458a3ce0222ed6535204d2e9d44b0ba1baf0499b6469e6697767eeaddf61b196eafbc0b29706e2515c0b534f9a3947714f4b6525b06880fcb81929830c10f6ac4d1d2457200fd7351c0b130b6fe53b3264a77e4f5d959dbb59619227c24fa6d37ee78ea72b258495030692262f5d33465da66d6a9e7905ccaba4e2a64b1986e6622641227e98590422071c5a597a1a1cfaef73ae10c03a12a501e34a5bb0c056cb541b78d6ee9d2d87057f5a914b09b0e1743789c6c21a6a3659083e1314fd08591152c1ec3530e4ff0f55bc6f3d9429f47f950076dbc4a4bb5b82563c036b0f1512b7f94db74fa1020dfd9ca1be2f2e3500ea0c3277eccfc9af2034ca57c3a76e9a7fe04dfe9610dd0d6c4e52aa8fd48cca8018e66cd45ee24b56d84ef3edcd8d14f78f835001e92d02a638b784c5a4819465d632589c01ba97bef1f3e5b2c28bcb72ac0a962a48f70925cdb445cccdf887347fd7851bc5be9a0c00914c12ca56fb8e69057552650d0428e4dfc694beae1a7e1c7cc294825d4da0e077b5a5e6b9e25c495dc602373e085c874da5079368d1016f1c13fb466ed79e45a96dacd549594bee8687221461021399058c4970a3e432ff56c8375181062a41b28f483e2a43406504cb93de3d038f5f9ae2bb6fd420099a60c5726adbcac553568254b391bc35e95f9079592306283cfa9fc2697f7fba9d4e493ce509bf9b930901233bcafbe8e79c5199e8650ae6236825df5d3a6365f2ea7c2481758df865aa069b3e2d94cd8ce4c0fade3c04c17c008b37b51b97cd8fd8b8d08f2944a443a34cca8dd66c27902b6c0bb03e09363b2acfde3bb8f5274bf57b03c1b5766fb6598ee6a4e98c583760264d7f140ad330e408f75c76c064ab68b81960b5b67805b71a4b075c4c47fc87163c061b00eddd1065213ac1eba09330c07daba241dc37024ff8f6e12e426a97a53c0ed30687e6c1a1558e27f03637580316265a0b998daa5edf5b99895e065c1b9647920f99542f723d2c1e3f0ceaa5b7783e492eae3311fd04a4a7f6295b0436a9ca9f06d478dc754b56629b88d756527bba358ff2cfdde2ce98a6535fb7e2542e8dc103db21cc05fa1c31f3cecccf16386769b3cf741f1cb974ff0f5900fa117f272b0132ae286fe18877f98cb820056797a73e780aa101a3bcf333c4226b01d1983e0e3e8401e2fd594f7975a3cddc673b6146c0274e598cc753b6cd89c079f1ef950f3434c0cb78a67e4c315dac5484624f2b26a58c1d3456a5cb77b79d80fba63b08dff6962488c456584fb3c430bf8644b715cabd707465cb6ae2cf85cdb9caa80870bc9c915b43382198d5e3f23c85cd42d0da4bf54c6c067c0e6d35efd14b040b7f4d3ea5f404b58bf91b2952fdd83b7726a897bd6c431c3f8e12052ae22ea40a7932b41c1dbf956d1d9262f196fae8ea12cbfdb0833cec974a14f954ca4bc507d87a3d10531ad0d9b6ca7379a91e95c285813a983a25120bb61180cbed53910227a7ea0f6c5c175b9198eaf2041c26ae27c54b40cc288650b9389a017cf8a50117367e01a8932b63f4808e153e7f530c50e6535117a48450939f3437f046800978be3f364ef99c212802fdc866d829df1c7d8066e686dd80e01f568492018a027ccf60c8f9234de49548b217277ee30e0772f37d32e5e9ce441f4f4d149430058c8bffa6563f51e8a3e80414a1a0f67a6312ee285f1f9011afaa9b380fa3d70864a7085cac322df9d87b26fc634c46e78c529cf2d8e2a058a10177d0a7a0fc041fa8ed5d68997761071b653da05b36b3cd056aa2d1be3f6fd74b9638f77fea00e0e20cb62ced28446f4197d15d20a63c332d75e7e47b6fa6652bd03cdca5a50e674f596d491400c35230269e16a796eb652daaf7ee938026ed2709f7df3cc70220bd06776fb86b9549e3fc6f91e0f9fe22cefbd8b181126382329f7195275f0ea3124c144c6d0743d9c69d68bf12693daf517a6c5006dc70659e9cb6053f25082023e5089b215c423907c43efb282105db94f95409bab9e2daa287741bf73e07fa18dabdb126af656cf2abe58d7d4347f7eb53ee335c71160189ef7098635e0413bc0af0f06746ae7fa94a8843cf9a246c346500d1efbe728822ab06d49de6093420a69f9c89a42a3d1a66b3d870a3d9cf1ef3887047426602b0777d847ab2059a62d72c832f5008ca5358752cdf83860bc972652f4a39d3f58074df745a420deb53e94cdcfafe794e77df270a05e90ad6712ecd0952b98d0fc5c04b4d42da0673236762a5a53a18eb5c78da44cd6a6d17e21a699eca52631fb09cf00cbf4108362981959477968ff2a8a492aea24c82102f2bf4996a33c297f784ab1f93c1003545213b6c32bac6968af83e5df1f280c4093cc49a8f4105ce2f57ff1e8ea80d91cbc882504ba080caabbb0653a1c29cd670e9f7a7998726871549a903908d02fa465831aa9e36a75c2099f65cc4ebde0788371baf35054fb80e7b6c8805b80e1133bdb22d614e210f9e2427ef025df0e7361a8aec9dfe381c5b351fd0fea00fe4a5def645b2e65443f5324c9d6f31fb3908f9616e9addf7a4e720cf4fbe2d0113f3bc0f0b8a05f5ca03a57efd3214c06b5a021f28e5467438ed8dc383e9ad01a75f33430630326fb552a041f0936a3808e99c72f18698de187f21747d4bce097a3736af7eea13679aeafbb61cea4446531fc011bae4313d2c4807c77a3a0307c89c20813d6939ae0774e4213afac54eafe7a1d192062b902899ee16cb5698092612508caa14cfaa72f85bd443b8c4fb43a37e0c2ceac4d640331e5e9fd4f60f5c410c26d7bb2bf2db6f776645b7a867cf18e37e9d03fab7cff74cf461be9b00b417547ba0368c497139c65de10cc9e09701b4460a20830c7b06f6013a4a530d903f1ebae79a059341826e696ba7d8d979c883c255d269b69a5e75b66154e303d34af0fcfc57212ca9b6d8582b148f6ce8062d3cc4eaccb7d1a5ae98aedf8307306b51b8f9cb5f7aaf4c4b27a63677f497fac65fa049aa856a7eaba520d76c09b37c629e254137e9dbe10db445d7fc9e1a20ec9ea73a4045a55da1c0d939970ec146e38cad95a5e6bb6e8855ecad1fbe65b2c3b2abeff02f52e2544525611c0d7c375559cd0491d12140520dbe24263fc5756d7ecee21b7fe7351d9a41baa70c2132cfff19cab674797ccabe2fffb03c3893308ad7c74995275675cbc441e40544cbf6c70172fee3ce115ee27c30281de5264e9bf4f3fee91b35af45f4eb080e5744fe53d448222957bf013044821cd008d7e9c3cdd30993e9a0c5e9b47d100d65ae60d687779dbdbdff5672f808d24626ef3abb5ca3236abf4d5bf44d9b190219ae329859e39ec99555af51be209f22eff843bd6bb141788522bfde6276cd0106f786872b79afd140d15b9f5e0077c2e27608d8a3dc91f23b7a38f4ae54750c6d9facf4b754d4f71a90d004a02214401e0a0f932b66bd1b9a800a50cc4c870fe5e6f6562949ba4972a99f1556606c2e731b65164584245ce0c1b321bd9edc0783ef3cf00ec9994f1b77ff911bf1d048722a177f2419ba3016f7d26f12941b007fe5da4162b7729e0ec39d39d8cbd6a0ddb2a1336029a834fca7b21fa7cb51057e28b3fce915bc6c3f10aa73531a6e117db8d2e7f432ebebda458a7cda58aa0516e679af69435b8cfa2d25220948eb57970fc6ea1664110accfc81a79b98a401832e03d589992679140b87d9f56906a5df8092da3ef1aadf3af6de87552b1006874e02a63c4befdefb631bd4fb490304f1d0db014cce6895e93a83b86565c00e3a01b39275fff5651f06d137f5530d5423a19d6541509634d05ac3db3518e20c9a77949625e201df1ef317220f137b627397d69806bb68eb496b1c4f4580ab01dd111cd59de5f93918bcc302bde5deb2c1d2e6d2d3c4ced78c9735d0fbccc80517e4a75108c797afcce407dffefeabbde80fd70313771f91b54c0cda90874b0ce1a86d51a5cd7c469675cc71010533a338b7620056116c4450810d9d67c58e02ff06a78b58fe44d1f6b9819d48f56e14ef924bd9f660814a57957f34cb2d58038b804d343b880e4beaa4529082ec56b08334123f229f876972c92bedc1504203677799d7b063ab728965b7c3ceda36cd5fcd16f51e030247155a3229a6e15e0b1306d1e3919399389c10e19120aae49f5598ffb505f9e4442db746b067e67008439144beefb1d1a687714283a9e1ea2fa26365f0e858c6999460708f6789c201a549e6cfd7327a642d72b48a23c18d5e3e8160d5cf78c4a373d795408e8556093c0dbfec80a76a5bc0bd7f06459694767734c47e5bde62c0c2efe9c1e007fe0ee88a277f7949fea8b989c4f60360262ded9b9a739b28b6a30f63f6768696fc0a3cb19bb8c57f520a62ee13476c9a975dedad480033efed154b037e4426ecf1004a3203aa8b631a4658c32267f0e47b600f0fd04a834fc1e7daec465d00b1be06251156c8747d407e851aeee3bb8876a20345c51e7a030bd7c88ba4f5eccee80c372ece2e5ae0533d7a7d36697c1d68321b542d5f9c03ab14a79927188871a60c047a44315d084089ade9616b8a9e690ab9a002c3f3d3dd4f4b3dc9dfecb33508567ff9062a25e5b14272ad76d49838b035714a59524f631402666bd7458c340c6582a86c5c596272ef2c90e09c3e78f4c78e38a6deaf3bedd5a462c2a79dcc08cbfba93ca1c2f99c9f1e7170189ef2ce81de294d321a98fa5995d0c6f4891d0bc6b335b48b7e4f8cda33f55e9f17ee8c842665746ec88e71537c8b7581c4bb02f7c2a6941ac5db1ed9419866c9271bd77c6d626f7a1d32289859a8103ab1d5090998a08c7756f4ee84807a45d2c3a6a51cd8c01604bc600fcaf6a7d752ae10064f8bd3bd2b537469ade7cee2ea0af057c3ab138662e892c77d279ba482c5b806db9d674c4236d103aaba0ed84d775c697ef12b6f46ee56eab8d9ea00e21ecf0bfdc7f180d9f3d55b56dc7d2f388a54d38827a9e1d11cd6d4141c1a1e3374d205112f5ff0280c2ad91253040b1c7e1dccd5c600a0110f064a45c48af38767680693d9e64d036d75a68607f0c7cdd2723a1ab0edc5eb53c318fd524f6432c4ff088f5e6ad692d697fb10eed10fa0709750b18b3eb33073f86eb46ecb4337077b0442529213ee94fd59568efe0e22940a923656a654c48b137b120fb19d5fac81094decdbb3cb3a084c13017782b9762a7d8d39ea6a82c0de8a7317a8072b605b07faea7774a2ac9f634c27e2a6326d3047b4a697e7293eb8463f36fbc69cc1440725ac750196ab11523f206507b29bc7f20b17718d3271c9d57cd0d06b7dd60a00132dae793261a32f438ecda9e1ce653d4739d971e604942802eee20afef0a9008fdffd3abec52df552d3503a806a0e8dbd936fce7205adab9fa933cb76632b00ca668a38289d1d21ca817814e0cb6dfa6ff8e436d061cf305cd563a0f19b3604682fc0e0452100c8d190832cb2db07b4f703b913d282d08b5fd07164c041be00a65a3a6f6ca407bd211a6602b8b284747c3143e5778531fcf7d480e2461a2f0c7af908336cdd817f3c538acd2274ae3d155f8e85cbe86c3b32001db24f6a490d6389ad820fc1d290f687a13a1533ff1d155a7a51aa64f4870931f792b74a220753804abb5fcc8851d9ece737573f76fc9ad39ad3554acfd18d971db4c06e190dc0cefbfefb4b35fcc620fd29ca6b47690803ff8e6fd1458a8eacf60639fcb005f6361c772d73a9411725a1bd2f2bedebd2179c21f30224b5bf9b30b7ffd6350497f6ce201adf6c260c599c01a506265334a6c2480f765605dea20a5f32c8ba04f9f879bcc1e48aa1f4c745796d1cdbb8c77f8116c10c546668db3631d29b910ee45088e4a7cd397a46931e9d2b7bde5288673afd79e64b89f805f100361baa0ceb68ded94cbe062c7c5f7af6ceeecde9a8f2fe85c14a23855ca57d68ab6e97099a2e31c2b8141ca99aaff4409f5f56f1d2f9d6000bcc791d74b55ce9388c52039d82ec32be2639a0b950c2d5a0e5ffcd8bc04d36c3b226b4dfe29ad9da37b3030e6c9e0f2ff9617881d73b2a80db5396c7411f4ac82918b4e15986d4cb6c430bb6f58b473993a2eca4a151b57be8a36f65d4813a139509688074892a54aef901bb3c7b79e8a4272b6075bd1550347cfd48702e5271095326782199089a2ea10a0ea0aa9d82c4769d6e7826e6a462b1ebbe1c2684e6736d8350ed45d1a5f92d091d81859c985c0657ba47cb6508fa544750d62ae4f7f4477b049770bc9f651206ff93071accc81212b1af613593f6e8b7723e9e1eec62296c3de05c5d7d612502eb42570d23aed77e46e47cc0f45d3b7e551adf6cf5cbf2e7fd02565d982100087c6ae17f315c44e8c92e15a857f2eddb4c69c5fe654178a60a96770fd002d10c57fe09512ce9d06498c7d40b16bea1b25c38a9e2ebc82f8e3d53bebd9589bc088556c0715dcb167aa83d19a1c1e2e8dd169cb2c7efd5b83dc340e0829b8c09053b1d40985c1ef909aa2179567b1f010a26112649bb7e1f7ebedbee76c7c1f606107a909f8c85caaef0170877e367eeecc3d9aec5bb55385fb649fa534a797d0c07dfa6e294443bc822646aed3b2ce80f4d2aa67549de9b92bd684bb6ff8ce40ec6163cc6cd91c27d15b9210cbc5a280a7693b37aadb17ad85af1b69d106da60dde45149398e18e493b8638ff8bddd4f34a525302e57a516a94e5ee9e8d7d7a0f2299a26c10e32e59f1b21da69d21182744bc2aa3ecf761bf5159ec4f599ee700fba8365afb0cd4fad3c5020a61fc88f756ad8da83a11a106d5ab70160a0fa201145d0497c4e73acf0b30a91f98cc7ec3eaffe8b6d6812591e1e255c5674b5c07727ef8788dcaff511a0160455faade2a751d0f768ddd1a0f2136669bce4ab70083bfe6c2023f198a0a04021455cb69a0ce7dd71486bc77bdcb3e59734725960718b447b2c4466052a7e11d59a35beb5ca7a843c296af3c06379d8cbbfb46450f0510bb8d377fa0bbb1673d75f61e8acbe1e53e9699d66fac85598562abdd900242075de95618b13d3658ff88ed68ebdd696371ec197d0185ee375f599a04280c63d53c48b7d86c22c6ed8178b04f18905c289a02a5cafc0b380fb97fa07e7d0ed36e469b713dbcca8f8f488debd2c3026eeddaf255d978189d4d4525fcf3d0038d96082264d7df7d14005c70b4d767c9ef599e127ea492a053763e59b6043408f92c28a40746d54ee58f46e21fcb74d425dc113e749e20aef10ceba44c7b050f4dd035d6ac952e78bdd8c462e163b4cdc6be5d3847d12d281aae090763b0aa0b2bc13ad765435eafd2107d8ffdb864196e331155f9db0878ddee747a47e0600b77bd69fc5fc3f12657fdbaa613e1acb6b91a0f6db838c4df23ea4bb494b5550793e4135110d2021b0329dfbfbbeb915ad643f8f12083df6e8f3f061f7faa1b0843529e38c6f98a89ef5ee061cded2c9926839ea52f7391b767cf08e4af4bfa06dfd19be30662f5c2a443bde9569216016ba38138d3bf10ab960c2f47a0f8f10f81535b49a8e9932c2d8c06e22a9e103b6b5c0cd243ab864b52f4e2ded79c040eeb6221d7069514c22d4fd7b58ba9d2854b5ded0caf538b6e364d5f3aacb37d006fe5176a4cf434323282da3b2de40c1a1b49ad8614f8d393c20ba872abac310954a68e549f5e61167015f6a5c80b76f9e4de2429566dfe11d366bff6b7ea340e2b7927dafa02552c46473150cc17e6a601deeb424caa6dbd31d5d337e1c1e70b55c67bd9967b5da0fb8122a1dc79922229e0e7eed8b64f72c7dc6a0cad737001c7b714947c03e7c73b851a5e64d6733f183db8c8f5f9ef0a92bf5a52ec540909dd4d495a09bb04f8c89e11d6fd90c7351dcdb2c5edba228b207bfc6c30571d08c4a78ee277fdbc76866ab091298d82e48beccf3d205caeb9d6227c8be88cfa035d732bd6a885ab8c8ec222002f303511d3560615330887f26645d237f5ac8709342df08e53e03223c5fbe07ece791c3f2df73f47d5bc39e73c3b5bb4cce86b033628b191832c7e98c5a8427384a261cc362f10117705f51a0f0cd00bd352800224dea1cf7af6da04266c11a01d8143173e1dd582a8a52ca2c5b5c4244f289302a9f2dd55490358e872417e5a701c4db1c7d641a3aeee47ea654ab74d625fa9090f5ce59ec92d59d79868c324b9c5d4356330c186948d78c439f4407ff9e1be066408cb50390185af8e55ca9a341d4a5cb0128f2e89a412783c80d590ade60d0731643840b57c74010424b88eb3b55e2f6e1b6b3e489b138627d9e872166a1b0159f19698932f13c51c777ef0685c0c44dd289ab71b0bfaa6cb4f176e230b360815d7812eb1d9a9fc8e69010b4efa624c53932c6ebeb9e867847d3ff4bf6df50e452ac988e53b73abb5269477224423b3e2026673bdf5d6bfdd8bf53005bfa20df6690f30d7376f06db3b61467c4341a52b9bdce9ad0cfee5725cc1c5a990730ab0802effad1c22092546facc96eb3375c45ee8cb502fd85b0fcfb33474304d02d43e091f7fc96a538c799fb35231e141c81361617b40142abfe3bdfbac3ba309db87c86ce09316e9b30929751d1e144d1cd4b577f4843a2ef7398824cd135700f14f8487543e552801284ac0884ebd84aacf05c4b0e002892e705650888bae0c6109ea2b28463585caf9a0c85e8ff6a50a4eae459b75e9e43beafd97bf362204521052a2995fce8e5f047b0db32d99e3da653f39616ab3d2cc01d0b69830920570f5c3bfdd7411d1989f250043eabf69dd46b05001bfcdc344e819d637f7ec038aea948f197f27b9ff764cf3db5590a47a1fee4c9fe37237a93640456da9930fa0fe87c483546b403c261efdf8602a125852b5959afd5569909a5c1d506891081dc923d23f15f125570288287cef161ac124eac0651539b9757d279c3499360299a3e6c92a846ef5332c5c9ee18ff7a72c94d0c86117aede903bbe5d4171030797d0f97abdf8a579e68642305392b9b32903823490b337b4d665fbf0e8a73909808518960308c8f414f9bc284f6220f3b43f07e2eda53a78a864790166c14203aff4b45b0b3665ba59843a504254650a8ec6112efe131931db2cbc83fca30e0a03fc2173622417b4ad08145ed835ab9ed64a77550f20419282b3d98f4d805008ee533dc10f302079c1a44eb9fca5ba28a247d1ed45d5b0a682b87496f925010c040c0619149884bcb30e540b78aabf4aaa670c94d4c2a3989be05aad669a3b042f5a903350ff9db50bb791bd7b22c9d10b929fa224f4d2d971bd9f98e8da520f17d0fa700faca64115f7bda12fdaced172c0a5ca6f0d3c3d9498ebe0feab1808801d3fc024d2551de3ca2e3ebc61e9d0bb3468355fbdc59d18b13bbc95053403254c8a720719be443d2f5e6789ecb21c467787040a49e7d26fe8819f95d67e0577d8be0e483e940d9b5e68e06b70deb79cc3411bc98721e1ce5bdf377da7390fd74b49b4c38a7b991bc6ac259037f92afe82e06b20ff7abbd90cffb02c58a0065de8792cd3c77450f744495944ceac7dcb1dc9abcd2dec4955b816307c116406f7c9524519e6891dece25a375636edb74055ae3290e1ed8ff3d98e6091ab3307a805ce7b753ac9d9a626526e6f0b7932adfab2cad0e1acbf04877582ec935b01f77800e27cd34d59206704e758eadd4e430f04a82264b0752eb778d448ae98054f24d7d98543a79e4b96a36eea021a44d2e35aa1ee96f5655eb5dd1a5076440b7e1b7529fb8a63d3ace684594707cebc30a6d2356c360131aa9d1ad7cfe971078f69a2fe9a63b4a383efdb398087559e66f94aba2aa9bf283472dca9abd0580a9a4f7ff1f85cc2701f8b8a57a6cee0f8d8356a2d063583c2a46d2ed1bcb4930f10f43326d88e9c7fded61b8f874c3db5e0ce74988d1b0c878143f2dde201a70ad447ba53d27516c3f069edae78e21b82d59abbc34b9acd5652beb21e089c67033bbe50ec6ac6cdbbe12ffbdc42932075de680f3cc2ae18e98808d7c083f69e0448e747d8f36b2887c14cc6072e4540b55f05f0fac2b7f794a6040a43ff7a160fbef5998845e31fca23e5bfb22c6fcee3caed163a80fbd5b893b285d2fb6a910f74cb7f2333b6617ce487af4b87a07dfe2db5308446dc2d36a6de2d32fdd1ad0b3cf68e21601ac3bf23fd5effa00cfc2c92787b8edee4b629816626019dadd20e23f9a2dcd0bf9b0feb1abd9cca1583fa3e38204bde0c958cc3528ded8ac7de093f9cf3544ba4dce3a943bb1213b3115b64293fd618deb01aaa7b90ae3dc5f60fcb30a81c0ed0f06937c83a3df75eca6877f0c488db64d1878d32087ed3ef7603ee930700da158569871bc2001242e385703f3cba03c4c966b5f06a5fd398660077200267a4d2fc92b4bc59fa8b05109f9556a964ee01eab2b4269c5e1af1ca04483ea0679231c848370fd0128329b9b0c2fe326addaa21c83be6489d4bc1ac00b41be91efa23ecb150877e15d677d8e469a4dc77a0b5f7cb58565518d1c5640923b94a28ca5c27a3d09e3c9b999913d356327d0fb065ad9b1f1f8e27587875086acad4e603a751a1c697c9a64f8255b27f3ce240930e64ffd071508fa76ee40ed08fd2dd01158c6727a5f3c17143b4f5fa40136d1827c5841de14fe037d38b0181bee1a6029ac3c1416323cabbc3e0a95b85ca7ec8216d3e1f45fc210aada9077c283e37aea921660d721e8e036f4b157016f7ff4c40aae7e0b7a11f4a9e610c5852fcb1d06b000405153fd341fe4f6b0a0c0b7bd5780496ac6b542c440b040ef15d1c1396290e2e9d54609592367511b6077a3427b2561d6af598ed2bd4eb0328232eb644c490c42ff1a245b2b47e71b0685d49f2af0b65cc9f73bb3c85200b711d36af06633265f726afa085d1f661fdf5d2f7924a7d264ad5ec8726b40f020d83b05c65e9b6c5bd455fd0dba69a512d526ca4449cfb36f0eabe174dff180fad3dc10dbbc5a13dda0c46b94c09f4a1b6c47583b72e5e45d282417ba8d23d00bdc200742e410167105563a59c08cd6299eeb74d18e4db32d7dd87cc9f09e20d635dfa6086b2959b60fc9e161b2025d832b788feda1176af5a0bc86bfc9cb6081e1d0ee48b45d95e1467adcf3d5fadb9ffc7ee049066909dadabae16fdaf920c0220fac965a3fa34576f43a5307d55b3949d6d4b34952775bf71ff4e075ab70fa38c63370c35d5c044b13e32c07e5c3db37be9f9279ff87b08b472d2262ebf0a12bda748361624edf05dcea3a02d81e522929183d2cedc3534c8f8f822bad904d95514645249676c427a4ca8fcc2c871304fbd7d28ba92f5014d7338f1571807bac768dab0709357d3da3c59a4a6faf06387cb4dce86ca2429cfb4b3bdfb4404110e382e1c97c91fb1abb519ba4eff23fcc2414abde45ed04d25a63734fa5d000e31628fd4593dd24f2c5dab9b487ef64abe41ec9c3ffb2386e049d864b9580c7fefcc65e8d2103694dc899ed6f7766a033c4d4d2849215c2bbb01f13ba6760b515f067998c8de1ee8df50e604b13483774eb023fe8b004c96cba3e85ce54d0b80858066f2bc0649f25b86a459c7b85d4d8f1bec43a772ccd28b068febf827094385e2b94e0d001925449676bc1899ae463f9c815716d74103203524a896f5067ee0d441641ba8464bc19c1a9aa549ad1fcf6fe85313d6bec97be9b642dd6b058ee7187e4d6c951712965a0aff81701b73f7b86d6c573b6c7bbc334a98aa7b0146ea57ba87550ed6e17577d32030868b5508a1011b508696cb16a29953238d092b7d17dc6b1d1c6c202899535ae91e30735c312c84aea93719c74aae4168a20ef425caf163039e7dd2a22a7774b6f0b71eda2e1ee98dcda2a420ea73f81f160848ca9568944bfe69b5f891a41c736a7bce9e327e3bd94ee8ac532d118c8f0204ade2285a08a36de6815e2b652eb1f770e08d2476bf5afd2b61270b406a54270b47d2ed113b52bbf4a558337bc777f63567c95b184eb83fa09f73a7a994132a0377e569249c794eae99ab0c8726cdc4c8ccc655f4802b38de61d14f3a75f4a20c6e25d919fd08f4a8d3a491db586bd2a267d363669c93bf6427c4550492b60f057aa02477cf20a5f21fcc7dff035ca63e134ad0e0f0e0e5c4878166c6ab204a0a79b120b106e7bdb228b5b0a8fc84e89b1d88e1e8a5812ddc7075476899363c0c60842c01eb7f75f2e2ee6c100a3d0d431d14fe2e8ac9a39a1ee48c65dbf225038143623625a9c06d99a3a1dbf1c9e94b60bcef697088f2ca8126a939d926490792547386d0053e34ba42c9d43fcec593bd879bf857666a7473ad29e5dc7c9c0146a561cbf612cf9b7d8d061aac87c0b68582ade4816b5da4b02de5e242984b0cfaceb082b3437f254e5748daafcc5d53b39039ea6638cca039ba7294faf5400bbd0f326740fc71755986c7964f54c4722a69508400f89494aa0754acaefa2200163b42a71d87454a0c9c9e224971ab4a7f0cf45d998715439566398ae1964d0b9a58af94af2b2e80734249fb36378ec00337c1be82163f9239f5a6a34b85f7003fed671cf50ebbcff41bbe6f9fdd6d02d451c02623adf37b4161601d8711640c2c5ae444c99d89bfbd3137ebd073e50c6fa79dc9ef39395b628b445f5c38a1004b196f5e8634f78db3a70a40b5bc4d08ace83bda4af8692567b4e346e7f8aa0962e36abd80adb6f2792aa2cea06bee39d17191033dec446b737bbec5c9a0580871fb3172a697bba3d041442542568ce649ae191ab33dd4b553691c6f51551009e9fee145cfc0e99aed66a7cb7ac5b24d91f4b3bd21c9031cd012857db2660e0f46a1cffc39f98008af4fe1e0eb52423dcd0553c149bfae7a60655377a23c6e050f459499c9ebee4304f4fd877cb675a12a9a0813defc5e6a09df93d89c08fb0160954081d196b5f0026d9f5e8d625c36ffb1cf9b79c1a257f8548f8dea0c5f083defc48788dee33667b2d650a13de0aabb67c2c73940b740a976b742e98c6c0b9554cdac22b19c4457097efb47ba72fc9c9e826383659e65605d311763820f0b60cd9330d77457a3f0cd8cf7df8ef123c7f60d8d0d121e0dfeb185577b251a0dbc916a97ac3281ad3b0ccfe497dd82c99e870f49b831988a886c1f5ca4141c03e63fe7ee76b5d1286a4daeb2615c88a391b53380f1a5bf1d95e04f22177a83070dd795989af041a670c4dc52c0da4001e09cf340536b96b7711eecbba5c8430f402e258a704325fd0f1e600ec440be0ff7d1838d09c1a2ffed3b457cdadb2f03dadb6d3c8cbd428acaa5e6001e6ea872cf5cde5938141b675c086209f96614074be21a64951857c339b9ecf416eaa8fe8d473d9d9b78bd85670c8e10f2ba5f0759107b57acd33c7405ca1cb1cb1723ffc117e5ba0cd141d5d26a631f79b7210b7ad52ddef3969b696e22133cf3ef2d50e58bc48474dafbf66fa6eafc46e02b070c40ea4790246abb63cc89083ad713c3f0350c7cabff4dd3dfa4754e8c2a5205ee305fdd29f3f5dd7470deb537ada7ea7deaa649e244bef5a8f3b38b304bb006780e83f66dc2bc1423b57ea36843e8dfa6b7e86a2769679018c22d2f9cf6e20649eba72b5ca058582500bc8730e681e202a4aa3b029dabbfd6f435da881b7600fa82a91304805aa83e572f4d8717310c1cb4e9f7f51a5223740acf77dd66f804fcfba94bc5c14dd91a6b73cc4fa10339589a6eca38732b38d9778e446576370f01d9c88115395903008fc03528f2b44398fb4ceefbd9d9414796ced8bbee9405317210e5489eadbaed5c8038cab5b1a8d0ad2a0dd55c8a8750fc2ff2052aaa0e13b5a0335d7fb9146c5edc543c321fc5b5bb0393e48506c82b839765f1bf2503eb0aafa46d22097c10c0b272883b5a1aef32b058b3973a2b643640445cb997048b0ec9fd79cb99dfb344d6c031947473876a00fdf7e734d51c8abf72e49320071a6ad6ff22fbb906eda52fa70084e68bcc4d6e1d3dc2dd5993f0594db87c3e06e383fa9fab4a960caf3ad3b38ca6db2ff1e0d0409b3ceef13fceb699acb8b801fad2c1f950c24225c4a9f6f06f4be8a9128239609b9576052d3f357e40975108f54c41adeba889850c290c4e7d44cfb953053c7f71b793526ac891931798200b25e1154617219644c2c0ad0dd4a29bfa991956f38e461c9bda8ff18699e32d0987223e9d5c5688310a464c684fccff97e3c0f9515f64890f05ac572154041804cd4fca52563118dade866408a8ad7139576e74873ba6ca72deed6313e818ff044941933bd6f4ae53706dbccab257bf0c1e49a169f147edf1e1524f4ac9ca640b83504fc809b1556ed3d24e4d70c081fc77ffa93ba7375980fbe6fbf4d4af8508fac5a51ccea6c9c76f3687310380428c04e553be6c5ab941ac8ac677e1a7d1035c350dadefd2f1639ef79b88451e856f3e0c005fab477ab21cfca271faf07d08c95d7d485007441aada5f7a9750c2a8f8f0979e60955152fdb3701196be0420b3c94ddde2c5324b60ee5eb95532df7c299817c57acdbca0bf1a10c98b3dc140cc5e981d097122da7d1bdcf2636cdea0908173b6a629ea6a37da7549a6a015507b63875dc43feb49b9af19a2878c8497318308443e3206614a2d4f4c7c22eb10ad201f29bc1a55465aa81654c10ffb5ee79a45bbe8ccb1e8e691a17472b252008f6d60a75673b9708903b15158d8de098596bb7a77dd9452479a8c11d4c61cb009cd49e667ef1eab89d193cbab418dadaddff1c5d9a0b5afa7262c1f3f02dcd09f26519c49db069aa7d6626ec79d74ed6e13dea532d006ef6b1b71ebeadeb80031b4d6db4a53a3b68b478e5c15e44a6dc719bdc84bdc5a8b3691cafef66bb060b6b5106d81c44a7f8b2cd0391fc99c2a9a6e3eaf93d1352ec368280104bcb590705db321c98cd4be15cd519a36f40a0cdbe57d4a022039adead487cca2118a40f31dfd594a83ce8b7aaef29fe34ff41e5f3e571372a5873eba186209e0f2ef00f03e96022005078f3576952bbd014f81e6569d854f7446d244ce13d9dde5824035ae71cebd206704ca26406b6e44b9288ed37276341b0b58bde1a0a7f7279140cee7071159b75d3c51ed1224e113fccbe84a0df0ce6d3952ac5917a63cd78f9008253b444e0d6135d7c494ca95ffefc3166e77d968fa8a4e2cdbef009dfcd240425fb5026fc3f858c6368ca16e06cee27b5ea0ed17b2ac4c1b906d6ff24dfe0061bba63c703137c9290b8c155c5dcf0c811ccf3c227bd7a959201afa3acc1ea041353038a5332cce6333aa531817b635764c6432fe454c9fa12d86a599b28db0ce5583b3eda5e1900769c95bffeffe07dfe0372373f23d64f8e967fcf895c3e0a098131029a3d753f51a03716539e9bfb3e3be6a1a65f1a04068f4a43b33a3a0e2f8dc6042c6507b80681b7d7b8754ccf3f399d281e5fe7856b32fcc39db4b6026200d90f7232bcf863ff1255098d476f2befb7456dd7660c7dc25b8e55b0120719ebc9d2a8dcd82ce8be839fe3b2afc615d317f9edd79da22146bffa11813402c3f088a66d99e5cc510ce38f37296ba485c26ea364e3a9b56954ff6ecef00f0c967b3de19a62a251a6cc11c9ff0c98562d0dc98a9919d9007619473c9d8d07088cf86df307d33bbd9b17822418891ab732f31826333d06ec0d0ba74b91f7350752ed98c28c512e98902d1004bd3e4eee7b8bcebd5b992500c25e7c1c1c2ca203ac9d7d5bc3d8a5de0a71e0fa16c5774b2a87655713d6ee1f9fb62998cbf0e306abafb5b5d91f35ac7791f6487b524e95181d203cb4e43d1e5906fea9e91f580c67e01ad955fcccdc925726af6cccd41e98f572613b87f967200563622f07050f8a612fa7ad1e4324b7dbad3b90bf510c57ff29e2a7190fee29d7df3be6bd6000b815eb40321a72c27d5ff64148e1d3803dba0e2cb87eb9ef55382243448d0d00537399bd23e6921f28a8290b37b2604d8521f878851eaacc14738a602fc25a0e457e4b4d5be617c43f98d21dd059b080cab6fb954fe9db8ab8dcd8f4e6d5260651e8f182c61b116011c27515a097d7a39d192e753cea2404271d4357948e440c1583de2a65ac68e20ef0e6f131485049cb083d4799934b92cb2339b84612e400754b157e2bb0a23c0730553dec644a3eed6c63729dbd62abba39058b51479303300d7e05ba7233e5dbc99c3b074de41c839410c8303e505af522b9be3001280e2b860fd4ae9708d40c2bc5987ea23b9d8dde7903deafab8fc2ad315635ec260299ad6242474339d70a1d05816583e64f66b398a71f3f1269c5740d29e5c8f309badc151fc2cdf4f6ef5da4ae5279183a706c70485879359dff348e1981786b032881dd913de1be14c12ce00ca3d6b14bd2d0a0e070a8d383c12dead9e49f980a7da6e5e7e2885e137e72569043c3e1498ef57659033ff85b9aabf37f0418a50bcd483234671653f16aa5c261d4b18e7de6977a4e927699365ad135714925ac038849f6702bf164e91f0b791880aa53fcf711b90d0dec3be3cb9a2641e23b1603990f47e01391232ca798c312e3dd409a6dee88c3666cd41da96145a3ededff0d05e9952ed0b4166f8119dae6d25b37f47d6dfe416ebe2a94079d860f023ca00c63dfc3ded69ba9b290c1d3ea81c2d2fbb449c213230b9ca76ba2be49e3f504053fd262e8d0268d2a8eeb76fa139c876ae05e37e59aff139032ec5ae0cdbfc10b9dfa5716f629d3b0bd93d49f50c73208fe3836ea284501f704343c0875f89c0ea2e2eabf4efaa2abcd00d0eebb77a7cbd33d99000f9d2ef486d50f671d20710cedc134391a9c13d5f4f3537fc02f89ee640c82e4ba52d873b4d92faab8d2d50ca653b96c47604f51fc5c8d2a3ce71da7639f86e6e378aff5bf651b85b335c30f0f692c82904cc1a77f45fb31a0daad87c8202016add2d673c7e04837fb8bba08f362ad489b627f645a7c78601826062b33f19740b6efc8dbeb28ce5947554e0507d53aa01bb37708ce6faf31a1d5b28c2266675ae87b957f00cee51feb974600f540046173ce79f52c837ef9ef5d884a1c440f2919dfca406e6fbc4f61abc80371e523bb913368e84804565be43717cc8735f550d47128b5538fa6ce3911a900e11529a4456980c3fadd4e6e929418e61ef866eb8066c6856e1c0b1de44e260c25c453adf17de255727896b65c2e1e432030eba916961298eea74b84c0ec2c0c9cc8b7809f48985ce096d0df1e9e78995ecf08bf6e05f718d1f9b6ec2be679048cdba58fb1a36fa50b46a4ce5653e18d7ed887bfd66490943b92e01a7e58fb0cb8dbeb379d7847ad1e85a81e2c3166d7d6c0654349fa445914ed096ccd1cec06e379c2301ecdf79f9b5525d95704710350c49e95c24474079a7afc21c448e107bd1cbe5d605adfef366aaa2c91750145416b996b9f07e73098c70147b51cb90d90a628b89b6d4df483c570153e464d7d9498b1abb47fe050f7a814f79a8ef209f8d6fac58f858bd7b5981c70975f231eeeb20f8c69ad33279f43528a8406f80a414a5fe0775aa5f763faffea74c56a6511f81fae9d6f4448f32ddcbaacd61d0345d4a7c98360a323a6d433a7028ad5a6160a770c908cd2600cf3a317b996d90ddb80294c451ddf345d687b02dd75304d2af04b28480c686df56c8a6eff30db011402c9d3cc1c23698b5a4802f8c98d6151594072a7b5c5deaef99a6f91f6ee0f0befd78190fb6b09004e4626499735999dbe9b0fc7b7fb59731757ace030b00b3d57db8d7214bb5556a13a06f851d86b5c5dbd530b93e95613fe7c0087ec9b0f60844ab9da597a202c7e7b1b4fed9508528a1b50acf7d5bb4db5ce78b6dd9e063a4a53b3b06c235bb04e3e37457018676ea86d986cc19d260a88c15b5d338403f8b8cdc91ffa1539874d333922a2b874df6fa682ee989beb19cbe55b2b12f207533a0550554465d891c16fd3042e27ce53fc47edfd8110b841c0a9a796ced4080a1fa0e46c7beb36b96e9060aab9ae09bc7d673ad5e69603b36d2753100cc50ca1fa9ed816fd2f123768e7077a90c97f166fd3f4e89dc72137ae881d04d9e3073b2f1c851f4a85b37801288f67693625a8d412cc5b6986193b10e4c70206d403af1491ffbc9334bde3168adb488101d9cb53116eb8a45c7018d603e6607c0d0b84841145327cdfae6b22bf7a77deccac02eec70806d74dc8c3e4dcd96b772307ecf65ca8b5e2446e67307e3c2bb26bcab4ff9b2104ad6c95245e39890e2f3d0a8efb3bd092150e3c11624c063007f34366c8be3d05bed1d5f697403a3f505b024ec603319da9c690b699cfe9e61783907a7eaeed71bd7af442ab60824527cc0a06d8e7ab2262a8935c69a1ae782f551f3ceb37017faf87747069089f7077c40b6b2fbb3b27392828f42950455ec1f5347f08c8a8612666f8ed6511323615db01860fbd22a0e4bcf30e5eaf944aed804b47b2595c6d72affdbfca165f50bd16058d80e5363cd7522d694f6a057cf7a4ae83b7f897c7c6c2569ed8c3ad4f0d3e098ae5c2a96cc6dd6af19aaaf7235f0727005f727d93999b24a0d91354e6bf2c0e7f46f2ee0154185f2751d0deffb4d7bd10940d305e136a58be79d7cacc57db015438d4d65d3ffa0a238e38ca55649e87e43589a3604be79aebd1292c8528070915ae108a1e861cfda8ff43424eaa3e474793296b2fa53d16957cf58283bc700f7f356355da975a1828150569a1ec3a19f348ac658cdad21cfcf6f596b30ae70a4a25c11b2903ca7cfbc149144bd418ca672dce17eb1caa101799a151553f2107df0b49f578ca8c6bdd10902cdc3e33b6b3fda35e987814fc0bae82233ec84f04c01f825c74b97049cb781734e5a1fb0a813c250be2b2927d53d97df771c32e02"))))

(test generate-ring-signature
  (dotimes (i 10)
    (let* ((secret-key (generate-secret-key))
           (public-key (secret-key->public-key secret-key))
           (key-image (compute-key-image secret-key public-key))
           (data (ironclad:random-data 32))
           (ring-size (+ 3 (ironclad:strong-random 8)))
           (secret-index (ironclad:strong-random ring-size))
           (public-keys (concatenate 'vector
                                     (iter (repeat secret-index)
                                           (collect (secret-key->public-key (generate-secret-key))))
                                     (vector public-key)
                                     (iter (repeat (- ring-size secret-index 1))
                                           (collect (secret-key->public-key (generate-secret-key)))))))
      (is-true (valid-ring-signature-p data public-keys key-image (generate-ring-signature data public-keys secret-key secret-index))))))

(test compute-multisig-blinded-secret
  (let ((secret-key (hex-string->bytes "6f5631bfa0a7a4719cd2ddf3acf9ab26a93c88dc5560ebf8e426a544ccad8803")))
    (is (string-equal "37086110d56edb0d244128eee6980255f427acca9b965fe4a8a26d17ff8be005"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "ddf53de98c2cde52a5daac71a7464dc4f46d7d54afad4259952ae3c97f663305")))
    (is (string-equal "2198d1a8f259eee41ef087eb48559edbd5841cce739709f79767adb3fbd25104"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "8bc2f1c91767c32404b5fc906122ee5a22efb8c5c8e229477850069fda334c04")))
    (is (string-equal "a8d18e7ce2e4de4c6213b9e883280ab0d6ee0c4640942151270125ffa71b500f"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "88a82e356a24ec0ec1dd497a3264c1c28741905781aecf051f89ec3dc962d101")))
    (is (string-equal "f4d31e959d1dfe34619da70dbecec62fb89c29125f094cc1fcc67fd0d6d60909"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "64bf5bdbbb82c6d084b1ae8eff4b0710b372e53465c0c9d90db1bbc8dcbb9c06")))
    (is (string-equal "8b9bc92fc3849735654c8e6be3fea89db96b8724b90cec21d0fea2485b062506"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "02dea042ab01454d9ca369a454cb0613c6f2c35d9009c3daf959b3d839570b02")))
    (is (string-equal "aede2ddc27f47417d81832ae44f917c9d33ad3ae5f4cadb2a9b8e8c55e7c9502"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "5e41a1e789066174019f1c82fab08cb7b7ca96965d972849c06258b79224b704")))
    (is (string-equal "f78e43b21da28affdaabcd00189c7f6a19458b2a695fe90b670ba8197c628b08"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "16f065708dc3f9bf6095ade6867dbd68da2968a668e67d05f14b9dcd2986f90b")))
    (is (string-equal "367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "0724632717bc4d9195c7d0b8c19565adaa88b8e8d62df899559eeef1d74d3e0c")))
    (is (string-equal "b3e6f3b9b75b5fc2097f076c54cbd66d0d8a2b4c01723dd3e48bf85c1b38e409"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key)))))
  (let ((secret-key (hex-string->bytes "6919108e1aa3f4402c670aa762e4554400fc3db0c26c12cc31f9e89b1b93a506")))
    (is (string-equal "5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c"
                      (bytes->hex-string (compute-multisig-blinded-secret secret-key))))))

(test compute-multisig-secret-view-key
  (let ((secret-view-keys (map 'vector
                               #'hex-string->bytes
                               #("37086110d56edb0d244128eee6980255f427acca9b965fe4a8a26d17ff8be005"
                                 "a8d18e7ce2e4de4c6213b9e883280ab0d6ee0c4640942151270125ffa71b500f"
                                 "8b9bc92fc3849735654c8e6be3fea89db96b8724b90cec21d0fea2485b062506"))))
    (is (string-equal "7da1c35f60753f381504789f6fc6d68d8482403595376d57a0a2355f02ae550b"
                      (bytes->hex-string (compute-multisig-secret-view-key secret-view-keys)))))
  (let ((secret-view-keys (map 'vector
                               #'hex-string->bytes
                               #("f78e43b21da28affdaabcd00189c7f6a19458b2a695fe90b670ba8197c628b08"
                                 "b3e6f3b9b75b5fc2097f076c54cbd66d0d8a2b4c01723dd3e48bf85c1b38e409"))))
    (is (string-equal "bda1410fbb9ad7690e8eddc98d6d77c326cfb6766ad126df4b97a076979a6f02"
                      (bytes->hex-string (compute-multisig-secret-view-key secret-view-keys))))))

(test compute-multisig-keys-n/n
  (let ((secret-spend-key (hex-string->bytes "16f065708dc3f9bf6095ade6867dbd68da2968a668e67d05f14b9dcd2986f90b")))
    (is (equalp #("367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900")
                (map 'vector
                     #'bytes->hex-string
                     (compute-multisig-keys-n/n secret-spend-key)))))
  (let ((secret-spend-key (hex-string->bytes "6919108e1aa3f4402c670aa762e4554400fc3db0c26c12cc31f9e89b1b93a506")))
    (is (equalp #("5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c")
                (map 'vector
                     #'bytes->hex-string
                     (compute-multisig-keys-n/n secret-spend-key))))))

(test compute-multisig-keys-m/n
  (let ((public-spend-keys (map 'vector
                                #'hex-string->bytes
                                #("1892038bd30631aa5f0e056ae47f87b2ed9d72e32dc20b645ab0171d52884f42"
                                  "49022814b64909a8c20078877610d80add01fbf6ef9f55fc7c93b2b2f9029122")))
        (secret-spend-key (hex-string->bytes "ddf53de98c2cde52a5daac71a7464dc4f46d7d54afad4259952ae3c97f663305")))
    (is (equalp #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                  "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07")
                (map 'vector
                     #'bytes->hex-string
                     (compute-multisig-keys-m/n public-spend-keys secret-spend-key)))))
  (let ((public-spend-keys (map 'vector
                                #'hex-string->bytes
                                #("eafbc2265dca6fca729f038d00a408f03624f039ea10a9be1ef3d19aaed4e856"
                                  "49022814b64909a8c20078877610d80add01fbf6ef9f55fc7c93b2b2f9029122")))
        (secret-spend-key (hex-string->bytes "88a82e356a24ec0ec1dd497a3264c1c28741905781aecf051f89ec3dc962d101")))
    (is (equalp #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                  "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")
                (map 'vector
                     #'bytes->hex-string
                     (compute-multisig-keys-m/n public-spend-keys secret-spend-key)))))
  (let ((public-spend-keys (map 'vector
                                #'hex-string->bytes
                                #("eafbc2265dca6fca729f038d00a408f03624f039ea10a9be1ef3d19aaed4e856"
                                  "1892038bd30631aa5f0e056ae47f87b2ed9d72e32dc20b645ab0171d52884f42")))
        (secret-spend-key (hex-string->bytes "02dea042ab01454d9ca369a454cb0613c6f2c35d9009c3daf959b3d839570b02")))
    (is (equalp #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                  "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08")
                (map 'vector
                     #'bytes->hex-string
                     (compute-multisig-keys-m/n public-spend-keys secret-spend-key))))))

(test compute-multisig-secret-spend-key
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"))))
    (is (string-equal "a0e1217efc88057244e140eeea8d3279582bd540e9d895417e23d47869e29f09"
                      (bytes->hex-string (compute-multisig-secret-spend-key multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
    (is (string-equal "627716ec18a622171e639cdc8349be9e85c24a124f0ab301c763838b0db4400b"
                      (bytes->hex-string (compute-multisig-secret-spend-key multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
    (is (string-equal "a80d07f8df835312b683fc491b5042a42db171e26e4453ba0ad8b2142453ba0f"
                      (bytes->hex-string (compute-multisig-secret-spend-key multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900"))))
    (is (string-equal "367c0246bc866ac8e559cecd9f7890a9d96ec790b336347c5f56f99c00796900"
                      (bytes->hex-string (compute-multisig-secret-spend-key multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c"))))
    (is (string-equal "5b84e7b1469852fe1bf19f6057cfd72f22a84381d259f522d7f573112517750c"
                      (bytes->hex-string (compute-multisig-secret-spend-key multisig-keys))))))

(test compute-multisig-public-keys
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"))))
    (is (equalp #("a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71"
                  "f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a")
                (map 'vector #'bytes->hex-string (compute-multisig-public-keys multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("ada518b99a556a3b566070c0a943d739581e57b864cf7a449d57d277a9219302"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
    (is (equalp #("a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71"
                  "04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b")
                (map 'vector #'bytes->hex-string (compute-multisig-public-keys multisig-keys)))))
  (let ((multisig-keys (map 'vector
                            #'hex-string->bytes
                            #("f33b09c561339b36ee80d02d414a5b3f000d7e8884091bfde0cb0101c0c00c07"
                              "b5d1fd327e50b8dbc7022c1cda05e7642da4f359ea3a38bd290cb1136492ad08"))))
    (is (equalp #("f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a"
                  "04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b")
                (map 'vector #'bytes->hex-string (compute-multisig-public-keys multisig-keys))))))

(test compute-multisig-public-spend-key
  (let ((multisig-public-keys (map 'vector
                                   #'hex-string->bytes
                                   #("a768f72c3d0c4be6fc82329c99d1d66b8b5a2103c8ffd4f9011a860da6e5ac71"
                                     "f66d79e30d769593145edae7e2aa0ac8ebbe951a827ddab8ed4c9b9a593d185a"
                                     "04d8a58ac5eb2f33cbaac5faf350523d3a3acb77fa05092e2e02bef831993c7b"))))
    (is (string-equal "aeb37cb931ec04d0fabae0164451f268dce585a98c5110b2dac5efe199eb9373"
                      (bytes->hex-string (compute-multisig-public-spend-key multisig-public-keys)))))
  (let ((multisig-public-keys (map 'vector
                                   #'hex-string->bytes
                                   #("ce57400743c11f83b09b94ac3be9b23f5b9b756a3472d0fdf129f12475a8ebb8"
                                     "156d6f3561c4ff39deda45d6ef841246a8ef9a4836e4fd103d7fa4bb7feb7ced"))))
    (is (string-equal "2297d23c0c69d8cf76464277ceb31e6b0777750308ecfbc49667fd2ebea67bf0"
                      (bytes->hex-string (compute-multisig-public-spend-key multisig-public-keys))))))
