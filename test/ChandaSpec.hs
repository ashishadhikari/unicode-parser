module ChandaSpec where

import Test.Hspec
import Warna
import Akshara
import Gana
import Chanda

stringToMatra :: String -> [Matra]
stringToMatra = toMatra . toAkshara . lexer

stringToGana :: String -> [Gana]
stringToGana = toGana . stringToMatra

isChanda :: ([Gana] -> Bool) -> String -> Bool
isChanda fn = fn . toGana . toMatra . toAkshara . lexer

spec :: Spec
spec = do
  describe "Chanda shardoolwikridita" $ do
    it "one" $
      isChanda shardoolwikridita "इच्छा यो छ महेश अन्तिम जसै त्यो मृत्युशैया जली" `shouldBe` True
    it "two" $
      isChanda shardoolwikridita "अक्षस्त्रक्परशुम् गदेषु कुलिशम् पद्मम् धनुष्कुण्डिकाम्" `shouldBe` True
    it "three" $
      isChanda shardoolwikridita "इच्छा यो छ महेश" `shouldBe` False
  describe "Chanda bhujangaprayata" $ do
    it "one" $
      isChanda bhujangaprayata "चतुर्भिर्यकारैर्भुजङ्गप्रयातः" `shouldBe` True
