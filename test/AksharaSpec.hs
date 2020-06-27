module AksharaSpec where

import Test.Hspec
import WarnaHelper
import Akshara


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
  describe "Space" $ do
    it "should be skipped" $
      toAkshara [la, u, space, ta, a] `shouldBe` [
        Akshara [la] u Nothing [],
        Akshara [ta] a Nothing []
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
