module L4EngineSpec where

import Test.Hspec
import L4Engine

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "spacesToDigits" $ do
    it "replaces any spaces with a digit, the number of consecutive spaces" $ do
      spacesToDigits "pp  pp  " `shouldBe` "pp2pp2"

  describe "positionToFen" $ do
    it "returns a fen string for the given Position" $ do
      positionToFen startPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
