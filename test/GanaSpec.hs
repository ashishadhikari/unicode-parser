module GanaSpec where

import Test.Hspec
import Warna
import Akshara
import Gana

stringToSingleMatra :: String -> Matra
stringToSingleMatra c = toMatraSingle(head (toAkshara(lexer c)))

stringToMatra :: String -> [Matra]
stringToMatra = toMatra . toAkshara . lexer

stringToGana :: String -> [Gana]
stringToGana = toGana . stringToMatra

spec :: Spec
spec = do
  describe "Matra" $ do
    it "hraswa akshar" $
      stringToSingleMatra "क" `shouldBe` L
    it "deergha akshar" $
      stringToSingleMatra "का" `shouldBe` G
  describe "Gana" $ do
    it "Y gana" $
      stringToGana "यमाता" `shouldBe` [Y]
    it "M gana" $
      stringToGana "मातारा" `shouldBe` [M]
    it "T gana" $
      stringToGana "ताराज ता" `shouldBe` [T, Gana G]
    it "R gana" $
      stringToGana "राजभा" `shouldBe` [R]
    it "J gana" $
      stringToGana "जभान ता" `shouldBe` [J, Gana G]
    it "B gana" $
      stringToGana "भानस ता" `shouldBe` [B, Gana G]
    it "N gana" $
      stringToGana "नसल ता" `shouldBe` [N, Gana G]
    it "S gana" $
      stringToGana "सलगम्" `shouldBe` [S]
    it "LG matra" $
      stringToGana "लगम्" `shouldBe` [Gana L, Gana G]
    it "L matra" $
      stringToGana "ल ता" `shouldBe` [Gana L, Gana G]
    it "G matra" $
      stringToGana "गम्" `shouldBe` [Gana G]
    it "last matra should be guru even if it is laghu" $
      stringToGana "लल" `shouldBe` [Gana L, Gana G]
    it "ignorable chars should be ignored" $
      stringToGana "लल ॥ १ ॥" `shouldBe` [Gana L, Gana G]
    it "long sentence 1" $
      stringToGana "इच्छा यो छ महेश अन्तिम जसै त्यो मृत्युशैया जली" `shouldBe` [M, S, J, S, T, T, Gana G]
    it "long sentence 2" $
      stringToGana "अक्षस्त्रक्परशुम् गदेषु कुलिशम् पद्मम् धनुष्कुण्डिकाम्" `shouldBe` [M, S, J, S, T, T, Gana G]
