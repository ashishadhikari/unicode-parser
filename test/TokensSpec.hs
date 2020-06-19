module TokensSpec where

import Test.Hspec
import Tokens

spec :: Spec
spec = do
  describe "consonant and vowel marker" $ do
    it "क should be क् अ" $
      lexer "क" `shouldBe` [T_ka, T_a]
    it "का should be क् आ" $
      lexer "का" `shouldBe` [T_ka, T_aa]
  describe "consonant and halanta marker" $ do
    it "क् should be क्" $
      lexer "क्" `shouldBe` [T_ka]
    it "क्ष should be क् ष्" $
      lexer "क्ष्" `shouldBe` [T_ka, T_sha]
    it "त्त् should be त् त्" $
      lexer "त्त्" `shouldBe` [T_ta, T_ta]
  describe "samyuktakshar" $ do
    it "क्ष should be क् ष् अ" $
      lexer "क्ष" `shouldBe` [T_ka, T_sha, T_a]
    it "त्र should be त् र् अ" $
      lexer "त्र" `shouldBe` [T_ta, T_ra, T_a]
    it "ज्ञ should be ज् ञ् अ" $
      lexer "ज्ञ" `shouldBe` [T_ja, T_nya, T_a]
  describe "multiple words" $ do
    it "कति राम्रो should be क् अ त् इ <Space> र् आ म् र् ओ" $
      lexer "कति राम्रो" `shouldBe` [T_ka, T_a, T_ta, T_i, T_Space, T_ra, T_aa, T_ma, T_ra, T_o]
    it "मर्म्माणिते व्वर्म्मणाच्छादयामि" $
      lexer "मर्म्माणिते व्वर्म्मणाच्छादयामि" `shouldBe` [T_ma,T_a,T_ra,T_ma,T_ma,T_aa,T_Na,T_i,T_ta,T_e,T_Space,T_wa,T_wa,T_a,T_ra,T_ma,T_ma,T_a,T_Na,T_aa,T_ca,T_cha,T_aa,T_da,T_a,T_ya,T_aa,T_ma,T_i]
  describe "vowel marker" $ do
    it "का should be क् आ" $
      lexer "का" `shouldBe` [T_ka, T_aa]
    it "कि should be क् इ" $
      lexer "कि" `shouldBe` [T_ka, T_i]
    it "की should be क् ई" $
      lexer "की" `shouldBe` [T_ka, T_ii]
    it "कु should be क् उ" $
      lexer "कु" `shouldBe` [T_ka, T_u]
    it "कू should be क् ऊ" $
      lexer "कू" `shouldBe` [T_ka, T_uu]
    it "कृ should be क् ऋ" $
      lexer "कृ" `shouldBe` [T_ka, T_R]
    it "के should be क् ए" $
      lexer "के" `shouldBe` [T_ka, T_e]
    it "कै should be क् ऐ" $
      lexer "कै" `shouldBe` [T_ka, T_ai]
    it "को should be क् ओ" $
      lexer "को" `shouldBe` [T_ka, T_o]
    it "कौ should be क् औ" $
      lexer "कौ" `shouldBe` [T_ka, T_au]
  describe "unknown tokens" $ do
    it "ash should be U U U" $
      lexer "ash" `shouldBe` [T_Unknown, T_Unknown, T_Unknown]
    it "क1 should be क् अ U" $
      lexer "क1" `shouldBe` [T_ka, T_a, T_Unknown]
