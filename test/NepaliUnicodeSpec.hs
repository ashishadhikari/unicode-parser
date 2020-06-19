module NepaliUnicodeSpec where

import Test.Hspec
import NepaliUnicode

spec :: Spec
spec =
  describe "isStop" $ do
    it "returns true for Ka" $
      isStop 'क' `shouldBe` True

    it "returns true for Ma" $
      isStop 'म' `shouldBe` True

    it "returns false for Sha" $
      isStop 'श' `shouldBe` False
