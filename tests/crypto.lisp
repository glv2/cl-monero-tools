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
