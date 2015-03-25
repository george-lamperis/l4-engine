module MainSpec where

import Test.Hspec
import Main

main :: IO ()
main = hspec $ do
  describe "positionToFen" $ do
    it "returns the correct string for startPosition" $
      positionToFen startPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
