module Main where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Tests" $ do
      it "Test 1" $
        True `shouldBe` True
