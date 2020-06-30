module GanaSpec where

import Test.Hspec
import Warna
import Akshara
import Gana

stringToMatra :: String -> [Matra]
stringToMatra = toMatra . toAkshara . lexer

stringToGana :: String -> [Gana]
stringToGana = toGana . stringToMatra

spec :: Spec
spec = do
  describe "Matra" $ do
    it "hraswa akshar" $
      stringToMatra "क" `shouldBe` [L]
    it "deergha akshar" $
      stringToMatra "का" `shouldBe` [G]
  describe "Gana" $ do
    it "Y gana" $
      stringToGana "यमाता" `shouldBe` [Y]
    it "M gana" $
      stringToGana "मातारा" `shouldBe` [M]
    it "T gana" $
      stringToGana "ताराज" `shouldBe` [T]
    it "R gana" $
      stringToGana "राजभा" `shouldBe` [R]
    it "J gana" $
      stringToGana "जभान" `shouldBe` [J]
    it "B gana" $
      stringToGana "भानस" `shouldBe` [B]
    it "N gana" $
      stringToGana "नसल" `shouldBe` [N]
    it "S gana" $
      stringToGana "सलगम्" `shouldBe` [S]
    it "LG matra" $
      stringToGana "लगम्" `shouldBe` [Gana L, Gana G]
    it "GL matra" $
      stringToGana "गम्ल" `shouldBe` [Gana G, Gana L]
    it "L matra" $
      stringToGana "ल" `shouldBe` [Gana L]
    it "G matra" $
      stringToGana "गम्" `shouldBe` [Gana G]
    it "long sentence 1" $
      stringToGana "इच्छा यो छ महेश अन्तिम जसै त्यो मृत्युशैया जली" `shouldBe` [M, S, J, S, T, T, Gana G]
    it "long sentence 2" $
      stringToGana "अक्षस्त्रक्परशुम् गदेषु कुलिशम् पद्मम् धनुष्कुण्डिकाम्" `shouldBe` [M, S, J, S, T, T, Gana G]
