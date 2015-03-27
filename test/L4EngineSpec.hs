module L4EngineSpec where

import Test.Hspec
import L4Engine

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "positionToFen" $ do
    it "returns the correct string for startPosition" $ do
      positionToFen startPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
