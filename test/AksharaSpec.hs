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
  describe "Laghu" $ do
    it "just Hraswa vowel" $
      isLaghu (Akshara [] a Nothing []) Nothing `shouldBe` True
    it "Hraswa vowel with pre-consonant" $
      isLaghu (Akshara [ka] a Nothing []) Nothing `shouldBe` True
    it "Hraswa vowel with pre-consonant followed by just vowel" $
      isLaghu (Akshara [ka] a Nothing []) (Just (Akshara [] aa Nothing [])) `shouldBe` True
    it "Hraswa vowel with pre-consonant followed by vowel and one pre-consonant" $
      isLaghu (Akshara [ka] a Nothing []) (Just (Akshara [ta] aa Nothing [])) `shouldBe` True
  describe "Deergha" $ do
    it "just Deergha vowel" $
      isGuru (Akshara [] aa Nothing []) Nothing `shouldBe` True
    it "Deergha vowel with pre-consonant" $
      isGuru (Akshara [ka] ii Nothing []) Nothing `shouldBe` True
    it "Hraswa vowel with post-vowel marker" $
      isGuru (Akshara [] a (Just candrawindu) []) Nothing `shouldBe` True
    it "Hraswa vowel with post-consonant" $
      isGuru (Akshara [] a Nothing [ca]) Nothing `shouldBe` True
    it "Hraswa vowel, followed by an Akshara with two pre-consonants" $
      isGuru (Akshara [] a Nothing []) (Just (Akshara [ka, kha] a Nothing [])) `shouldBe` True
