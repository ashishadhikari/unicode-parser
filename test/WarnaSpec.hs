module WarnaSpec where

import Test.Hspec
import Warna

spec :: Spec
spec = do
  describe "consonant and vowel marker" $ do
    it "क should be क् अ" $
      lexer "क" `shouldBe` ['क', 'अ']
    it "का should be क् आ" $
      lexer "का" `shouldBe` ['क', 'आ']
  describe "consonant and halanta marker" $ do
    it "क् should be क्" $
      lexer "क्" `shouldBe` ['क']
    it "क्ष should be क् ष्" $
      lexer "क्ष्" `shouldBe` ['क', 'ष']
    it "त्त् should be त् त्" $
      lexer "त्त्" `shouldBe` ['त', 'त']
  describe "samyuktakshar" $ do
    it "क्ष should be क् ष् अ" $
      lexer "क्ष" `shouldBe` ['क', 'ष', 'अ']
    it "त्र should be त् र् अ" $
      lexer "त्र" `shouldBe` ['त', 'र', 'अ']
    it "ज्ञ should be ज् ञ् अ" $
      lexer "ज्ञ" `shouldBe` ['ज', 'ञ', 'अ']
  describe "multiple words" $ do
    it "कति राम्रो should be क् अ त् इ <Space> र् आ म् र् ओ" $
      lexer "कति राम्रो" `shouldBe` ['क', 'अ', 'त', 'इ', ' ', 'र', 'आ', 'म', 'र', 'ओ']
    it "मर्म्माणिते व्वर्म्मणाच्छादयामि" $
      lexer "मर्म्माणिते व्वर्म्मणाच्छादयामि" `shouldBe` ['म', 'अ', 'र', 'म', 'म', 'आ', 'ण', 'इ', 'त', 'ए', ' ']
        ++ [ 'व', 'व', 'अ', 'र', 'म', 'म', 'अ', 'ण', 'आ', 'च', 'छ', 'आ', 'द', 'अ', 'य', 'आ', 'म', 'इ']
  describe "vowel marker to vowel" $ do
    it "का should be क् आ" $
      lexer "का" `shouldBe` ['क', 'आ']
    it "कि should be क् इ" $
      lexer "कि" `shouldBe` ['क', 'इ']
    it "की should be क् ई" $
      lexer "की" `shouldBe` ['क', 'ई']
    it "कु should be क् उ" $
      lexer "कु" `shouldBe` ['क', 'उ']
    it "कू should be क् ऊ" $
      lexer "कू" `shouldBe` ['क', 'ऊ']
    it "कृ should be क् ऋ" $
      lexer "कृ" `shouldBe` ['क', 'ऋ']
    it "के should be क् ए" $
      lexer "के" `shouldBe` ['क', 'ए']
    it "कै should be क् ऐ" $
      lexer "कै" `shouldBe` ['क', 'ऐ']
    it "को should be क् ओ" $
      lexer "को" `shouldBe` ['क', 'ओ']
    it "कौ should be क् औ" $
      lexer "कौ" `shouldBe` ['क', 'औ']
  describe "post-vowel marker" $ do
    it "chandrawindu" $ do
      lexer "काँच" `shouldBe` ['क', 'आ', 'ँ', 'च', 'अ']
      lexer "मायाँ" `shouldBe` ['म', 'आ', 'य', 'आ', 'ँ']
    it "anuswar" $ do
      lexer "कंश" `shouldBe` ['क', 'अ', 'ं', 'श', 'अ']
      lexer "अंश" `shouldBe` ['अ', 'ं', 'श', 'अ']
    it "wisarga" $ do
      lexer "नमः" `shouldBe` ['न', 'अ', 'म', 'अ', 'ः']
      lexer "दुःख" `shouldBe` ['द', 'उ', 'ः', 'ख', 'अ']
  describe "ignorable chars" $ do
    it "लल ॥ १ ॥,:!ऽ" $
      lexer "ल्\x200Cल्\x200D ॥ १ ॥ ,।॥ऽ-:!@#%&*()\"'/“”‘’" `shouldBe` ['ल', ' ', 'ल']
  describe "unknown tokens" $ do
    it "ash should be U U U" $
      lexer "ash" `shouldBe` ['a', 's', 'h']
    it "क1 should be क् अ U" $
      lexer "क1" `shouldBe` ['क', 'अ', '1']
  describe "multiline text" $
    it "को\nआयो should be U U U" $
      multiLineLexer "को\nआयो" `shouldBe` [['क', 'ओ'], ['आ', 'य', 'ओ']]
