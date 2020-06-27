module WarnaSpec where

import Test.Hspec
import Warna
import WarnaHelper

spec :: Spec
spec = do
  describe "consonant and vowel marker" $ do
    it "क should be क् अ" $
      lexer "क" `shouldBe` [ka, a]
    it "का should be क् आ" $
      lexer "का" `shouldBe` [ka, aa]
  describe "consonant and halanta marker" $ do
    it "क् should be क्" $
      lexer "क्" `shouldBe` [ka]
    it "क्ष should be क् ष्" $
      lexer "क्ष्" `shouldBe` [ka, sha]
    it "त्त् should be त् त्" $
      lexer "त्त्" `shouldBe` [ta, ta]
  describe "samyuktakshar" $ do
    it "क्ष should be क् ष् अ" $
      lexer "क्ष" `shouldBe` [ka, sha, a]
    it "त्र should be त् र् अ" $
      lexer "त्र" `shouldBe` [ta, ra, a]
    it "ज्ञ should be ज् ञ् अ" $
      lexer "ज्ञ" `shouldBe` [ja, nya, a]
  describe "multiple words" $ do
    it "कति राम्रो should be क् अ त् इ <Space> र् आ म् र् ओ" $
      lexer "कति राम्रो" `shouldBe` [ka, a, ta, i, Space, ra, aa, ma, ra, o]
    it "मर्म्माणिते व्वर्म्मणाच्छादयामि" $
      lexer "मर्म्माणिते व्वर्म्मणाच्छादयामि" `shouldBe` [ma, a, ra, ma, ma, aa, nNa, i, ta, e, Space]
        ++ [ wa, wa, a, ra, ma, ma, a, nNa, aa, ca, cha, aa, da, a, ya, aa, ma, i]
  describe "vowel marker to vowel" $ do
    it "का should be क् आ" $
      lexer "का" `shouldBe` [ka, aa]
    it "कि should be क् इ" $
      lexer "कि" `shouldBe` [ka, i]
    it "की should be क् ई" $
      lexer "की" `shouldBe` [ka, ii]
    it "कु should be क् उ" $
      lexer "कु" `shouldBe` [ka, u]
    it "कू should be क् ऊ" $
      lexer "कू" `shouldBe` [ka, uu]
    it "कृ should be क् ऋ" $
      lexer "कृ" `shouldBe` [ka, r]
    it "के should be क् ए" $
      lexer "के" `shouldBe` [ka, e]
    it "कै should be क् ऐ" $
      lexer "कै" `shouldBe` [ka, ai]
    it "को should be क् ओ" $
      lexer "को" `shouldBe` [ka, o]
    it "कौ should be क् औ" $
      lexer "कौ" `shouldBe` [ka, au]
  describe "post-vowel marker" $ do
    it "chandrawindu" $ do
      lexer "काँच" `shouldBe` [ka, aa, candrawindu, ca, a]
      lexer "मायाँ" `shouldBe` [ma, aa, ya, aa, candrawindu]
    it "anuswar" $ do
      lexer "कंश" `shouldBe` [ka, a, anuswar, sSha, a]
      lexer "अंश" `shouldBe` [a, anuswar, sSha, a]
    it "wisarga" $ do
      lexer "नमः" `shouldBe` [na, a, ma, a, wisarga]
      lexer "दुःख" `shouldBe` [da, u, wisarga, kha, a]
  describe "unknown tokens" $ do
    it "ash should be U U U" $
      lexer "ash" `shouldBe` [Unknown 'a', Unknown 's', Unknown 'h']
    it "क1 should be क् अ U" $
      lexer "क1" `shouldBe` [ka, a, Unknown '1']
  describe "multiline text" $
    it "को\nआयो should be U U U" $
      multiLineLexer "को\nआयो" `shouldBe` [[ka, o], [aa, ya, o]]
