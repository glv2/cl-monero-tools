;;;; This file is part of monero-tools
;;;; Copyright 2016-2017 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :monero-tools-tests)


(def-suite mnemonic-tests
  :description "Unit tests for mnemonic seed functions."
  :in monero-tools-tests)

(in-suite mnemonic-tests)

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

(test encrypt-mnemonic-seed
  (is (string-equal "unbending bogeys bowling tender sorry upcoming textbook nozzle shackles vein sneeze cage sensible oaks fitting agreed upstairs onboard reunion obliged punch fifteen emotion firm sorry"
                    (encrypt-mnemonic-seed "dialect enough negative umbrella gourmet january tissue army sanity nugget against foxes nearby hobby alerts puddle unlikely noodles jolted citadel below vocal dauntless edited hobby"
                                           "SomePassword"
                                           :english)))
  (is (string-equal "juerga casero jarra masivo poeta dirigir grúa iris pésimo revés lógica caer lombriz gajo explicar parar emoción minero ceniza oveja olmo lucir mito negar iris"
                    (encrypt-mnemonic-seed "boca lunes activo padre agua recreo inicio razón pichón gallo este canica curva carro bozal puerta perfil crisis pequeño farsa jungla mueble orilla pasar recreo"
                                           "456uhbzerjkl951"
                                           :spanish)))

  (is (string-equal "いけん おどり うれしい うんちん つわり とのさま げこう なわとび こふう ぬくもり せつぶん いやす こうじ うすめる のせる いせかい せんさい おかわり たぶん とおる なっとう どうぐ てんし なふだ いけん"
                    (encrypt-mnemonic-seed "けまり こんいん ちへいせん おいかける だむる くいず ていねい しむける たまる いきる しゃこ げんぶつ そつえん げつようび あずかる せもたれ あんがい いちりゅう ぬいくぎ とさか あまり てれび てきとう ととのえる だむる"
                                           "キーがテーブルにあります"
                                           :japanese))))

(test decrypt-mnemonic-seed
  (is (string-equal "dialect enough negative umbrella gourmet january tissue army sanity nugget against foxes nearby hobby alerts puddle unlikely noodles jolted citadel below vocal dauntless edited hobby"
                    (decrypt-mnemonic-seed "unbending bogeys bowling tender sorry upcoming textbook nozzle shackles vein sneeze cage sensible oaks fitting agreed upstairs onboard reunion obliged punch fifteen emotion firm sorry"
                                           "SomePassword"
                                           :english)))
  (is (string-equal "boca lunes activo padre agua recreo inicio razón pichón gallo este canica curva carro bozal puerta perfil crisis pequeño farsa jungla mueble orilla pasar recreo"
                    (decrypt-mnemonic-seed "juerga casero jarra masivo poeta dirigir grúa iris pésimo revés lógica caer lombriz gajo explicar parar emoción minero ceniza oveja olmo lucir mito negar iris"
                                           "456uhbzerjkl951"
                                           :spanish)))
  (is (string-equal "けまり こんいん ちへいせん おいかける だむる くいず ていねい しむける たまる いきる しゃこ げんぶつ そつえん げつようび あずかる せもたれ あんがい いちりゅう ぬいくぎ とさか あまり てれび てきとう ととのえる だむる"
                    (decrypt-mnemonic-seed "いけん おどり うれしい うんちん つわり とのさま げこう なわとび こふう ぬくもり せつぶん いやす こうじ うすめる のせる いせかい せんさい おかわり たぶん とおる なっとう どうぐ てんし なふだ いけん"
                                           "キーがテーブルにあります"
                                           :japanese))))
