module AksharaSpec where

import Test.Hspec
import Warna
import Akshara

--TODO: clean up duplicate test setup

a = Hraswa 'अ'; aa = Deergha 'आ'
i = Hraswa 'इ'; ii = Deergha 'ई'
u = Hraswa 'उ'; uu = Deergha 'ऊ'
r = Hraswa 'ऋ'
e = Deergha 'ए'; ai = Deergha 'ऐ'
o = Deergha 'ओ'; au = Deergha 'औ'

candrawindu = PostVowelMarker 'ँ'
anuswar = PostVowelMarker 'ं'
wisarga = PostVowelMarker 'ः'

ka = Consonant 'क'
kha = Consonant 'ख'
ca = Consonant 'च'
cha = Consonant 'छ'
ja = Consonant 'ज'
nya = Consonant 'ञ'
nNa = Consonant 'ण'
ta = Consonant 'त'
da = Consonant 'द'
na = Consonant 'न'
ma = Consonant 'म'
ya = Consonant 'य'
ra = Consonant 'र'
wa = Consonant 'व'
sha = Consonant 'ष'
sSha = Consonant 'श'

spec :: Spec
spec = do
  describe "Akshara simple" $ do
    it "with pre-consonant and vowel" $
      toAkshara [ka, a] `shouldBe` [Akshara [ka] a Nothing []]
    it "with pre-consonant, vowel and post-vowel-marker" $
      toAkshara [ka, aa, candrawindu] `shouldBe` [Akshara [ka] aa (Just candrawindu) []]
    it "with pre-consonant, vowel, post-vowel-marker and post-consonant" $
      toAkshara [ka, aa, candrawindu, ka, ra, o] `shouldBe` [
        Akshara [ka] aa (Just candrawindu) [ka],
        Akshara [ra] o Nothing []
      ]
  describe "Akshara complex" $ do
    it "marmmanite" $
      toAkshara [ma, a, ra, ma, ma, aa, nNa, i, ta, e] `shouldBe` [
        Akshara [ma] a Nothing [ra, ma],
        Akshara [ma] aa Nothing [],
        Akshara [nNa] i Nothing [],
        Akshara [ta] e Nothing []
      ]
    it "wwarmmacchadayami" $
      toAkshara [wa, wa, a, ra, ma, ma, a, nNa, aa, ca, cha, aa, da, a, ya, aa, ma, i] `shouldBe` [
        Akshara [wa, wa] a Nothing [ra, ma],
        Akshara [ma] a Nothing [],
        Akshara [nNa] aa Nothing [ca],
        Akshara [cha] aa Nothing [],
        Akshara [da] a Nothing [],
        Akshara [ya] aa Nothing [],
        Akshara [ma] i Nothing []
      ]
