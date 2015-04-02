module L4EngineSpec where

import Test.Hspec
import L4Engine

-- The following test cases are from wikipedia:
-- http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
--
-- starting position:
-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
--
-- move 1: e2e4
-- rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
afterMove1 :: Position
afterMove1 = Position {pawns   = 0x00ff00001000ef00,
                       rooks   = 0x8100000000000081,
                       knights = 0x4200000000000042,
                       bishops = 0x2400000000000024,
                       queens  = 0x0800000000000008,
                       kings   = 0x1000000000000010,
                       black   = 0xffff000000000000,
                       white   = 0x000000001000efff,
                       whites_turn = False
                       }

-- move 2: c7c5
-- rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
afterMove2 :: Position
afterMove2 = Position {pawns   = 0x00fb00041000ef00,
                       rooks   = 0x8100000000000081,
                       knights = 0x4200000000000042,
                       bishops = 0x2400000000000024,
                       queens  = 0x0800000000000008,
                       kings   = 0x1000000000000010,
                       black   = 0xfffb000400000000,
                       white   = 0x000000001000efff,
                       whites_turn = True
                       }

-- move 3: g1f3
-- rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
afterMove3 :: Position
afterMove3 = Position {pawns   = 0x00fb00041000ef00,
                       rooks   = 0x8100000000000081,
                       knights = 0x4200000000200002,
                       bishops = 0x2400000000000024,
                       queens  = 0x0800000000000008,
                       kings   = 0x1000000000000010,
                       black   = 0xfffb000400000000,
                       white   = 0x000000001020efbf,
                       whites_turn = False
                       }


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "spacesToDigits" $ do
    it "replaces any spaces with a digit, the number of consecutive spaces" $ do
      spacesToDigits "pp  pp  " `shouldBe` "pp2pp2"
      spacesToDigits "        " `shouldBe` "8"
      spacesToDigits "p       " `shouldBe` "p7"
      spacesToDigits "p pp    " `shouldBe` "p1pp4"

  describe "piecePlacement" $ do
    it "returns the correct fen string for startPosition" $ do
      piecePlacement startPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    it "returns the correct fen string for afterMove1" $ do
      piecePlacement afterMove1 `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"

    it "returns the correct fen string for afterMove2" $ do
      piecePlacement afterMove2 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR"

    it "returns the correct fen string for afterMove3" $ do
      piecePlacement afterMove3 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R"

  describe "activeColor" $ do
    it "returns w for the startPosition" $ do
      activeColor startPosition `shouldBe` "w"

    it "returns b for afterMove1" $ do
      activeColor afterMove1 `shouldBe` "b"

  -- startPosition {changingValue = ...}

  describe "positionToFen" $ do
    it "returns the correct fen string for startPosition" $ do
      positionToFen startPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    it "returns the correct fen string for afterMove1" $ do
      positionToFen afterMove1 `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    it "returns the correct fen string for afterMove2" $ do
      positionToFen afterMove2 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

    it "returns the correct fen string for afterMove3" $ do
      positionToFen afterMove3 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
