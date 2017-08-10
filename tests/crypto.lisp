;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools/tests)


(def-suite crypto-tests
  :description "Unit tests for crypto functions."
  :in monero-tools-tests)

(in-suite crypto-tests)

(test fast-hash
  (flet ((fast-hash/hex (data)
           (bytes->hex-string (fast-hash (hex-string->bytes data)))))
    (is (string-equal "7591f4d8ff9d86ea44873e89a5fb6f380f4410be6206030010567ac9d0d4b0e1"
                      (fast-hash/hex "01009091e4aa05ff5fe4801727ed0c1b8b339e1a0054d75568fec6ba9c4346e88b10d59edbf6858b2b00008a63b2865b65b84d28bb31feb057b16a21e2eda4bf6cc6377e3310af04debe4a01")))))

(test slow-hash
  (flet ((slow-hash/hex (data)
           (bytes->hex-string (slow-hash (hex-string->bytes data)))))
    (is (string-equal "a70a96f64a266f0f59e4f67c4a92f24fe8237c1349f377fd2720c9e1f2970400"
                      (slow-hash/hex "01009091e4aa05ff5fe4801727ed0c1b8b339e1a0054d75568fec6ba9c4346e88b10d59edbf6858b2b00008a63b2865b65b84d28bb31feb057b16a21e2eda4bf6cc6377e3310af04debe4a01")))))

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
           (data (ironclad::random-data 150)))
      (is-true (valid-signature-p data public-key (generate-signature data secret-key))))))
