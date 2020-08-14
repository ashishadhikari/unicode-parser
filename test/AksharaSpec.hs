module AksharaSpec where

import Test.Hspec
import Akshara


spec :: Spec
spec = do
  describe "Akshara simple" $ do
    it "with pre-consonant and vowel" $
      toAkshara ['क', 'अ'] `shouldBe` [Akshara ['क'] 'अ' Nothing []]
    it "with pre-consonant, vowel and post-vowel-marker" $
      toAkshara ['क', 'आ', 'ँ'] `shouldBe` [Akshara ['क'] 'आ' (Just 'ँ') []]
    it "with pre-consonant, vowel, post-vowel-marker and post-consonant" $
      toAkshara ['क', 'आ', 'ँ', 'क', 'र', 'ओ'] `shouldBe` [
        Akshara ['क'] 'आ' (Just 'ँ') ['क'],
        Akshara ['र'] 'ओ' Nothing []
      ]
  describe "Space" $ do
    it "should be skipped" $
      toAkshara ['ल', 'उ', ' ', 'त', 'अ'] `shouldBe` [
        Akshara ['ल'] 'उ' Nothing [],
        Akshara ['त'] 'अ' Nothing []
      ]
  describe "Akshara complex" $ do
    it "marmmanite" $
      toAkshara ['म', 'अ', 'र', 'म', 'म', 'आ', 'ण', 'इ', 'त', 'ए'] `shouldBe` [
        Akshara ['म'] 'अ' Nothing ['र', 'म'],
        Akshara ['म'] 'आ' Nothing [],
        Akshara ['ण'] 'इ' Nothing [],
        Akshara ['त'] 'ए' Nothing []
      ]
    it "wwarmmacchadayami" $
      toAkshara ['व', 'व', 'अ', 'र', 'म', 'म', 'अ', 'ण', 'आ', 'च', 'छ', 'आ', 'द', 'अ', 'य', 'आ', 'म', 'इ'] `shouldBe` [
        Akshara ['व', 'व'] 'अ' Nothing ['र', 'म'],
        Akshara ['म'] 'अ' Nothing [],
        Akshara ['ण'] 'आ' Nothing ['च'],
        Akshara ['छ'] 'आ' Nothing [],
        Akshara ['द'] 'अ' Nothing [],
        Akshara ['य'] 'आ' Nothing [],
        Akshara ['म'] 'इ' Nothing []
      ]
  describe "Laghu" $ do
    it "Hraswa vowel" $
      isLaghu (Akshara [] 'अ' Nothing []) `shouldBe` True
    it "Hraswa vowel with candravindu" $
      isLaghu (Akshara [] 'अ' (Just 'ँ') []) `shouldBe` True
    it "Hraswa vowel with pre-consonant" $
      isLaghu (Akshara ['क'] 'अ' Nothing []) `shouldBe` True
    it "Hraswa vowel with pre-consonant and candravindu" $
      isLaghu (Akshara ['क'] 'अ' (Just 'ँ') []) `shouldBe` True
  describe "Deergha" $ do
    it "Deergha vowel" $
      isGuru (Akshara [] 'आ' Nothing []) `shouldBe` True
    it "Deergha vowel with pre-consonant" $
      isGuru (Akshara ['क'] 'ई' Nothing []) `shouldBe` True
    it "Hraswa vowel with post-vowel marker anuswar" $
      isGuru (Akshara [] 'अ' (Just 'ं') []) `shouldBe` True
    it "Hraswa vowel with post-vowel marker visarga" $
      isGuru (Akshara [] 'अ' (Just 'ः') []) `shouldBe` True
    it "Hraswa vowel with post-consonant" $
      isGuru (Akshara [] 'अ' Nothing ['च']) `shouldBe` True
