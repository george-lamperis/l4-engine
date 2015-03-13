module Main where

import Data.Word (Word64)
import Data.Bits
import Numeric (showHex)
import Text.Printf

type Bitboard = Word64

data Position = Position { pawns :: Bitboard
                         , rooks :: Bitboard
                         , knights :: Bitboard
                         , bishops :: Bitboard
                         , queens :: Bitboard
                         , king :: Bitboard
                         , black :: Bitboard
                         , white :: Bitboard
                         , whites_turn :: Bool
                         } deriving (Show, Eq)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King | NotOccupied
             deriving (Show)

startPosition :: Position
startPosition = Position {pawns   = 0x00ff00000000ff00,
                          rooks   = 0x8100000000000081,
                          knights = 0x4200000000000042,
                          bishops = 0x2400000000000024,
                          queens  = 0x0800000000000008,
                          king    = 0x1000000000000010,
                          black   = 0xFFFF000000000000,
                          white   = 0x000000000000FFFF,
                          whites_turn = True
                         }

--blackPawns :: Position => Bitboard
--blackPawns pos = (pawns pos) & (black pos)

pieceAtIndex :: Position => Int -> Piece
pieceAtIndex pos idx
    | testBit (pawns pos) idx = Pawn
    | otherwise = NotOccupied

-- Returns an ASCII chessboard graphic of the position.
prettyShow :: Position => String
prettyShow pos =
    let indices = [8*x + y | x <- reverse [0..7], y <- [0..7]]
    in  "\t  +---+---+---+---+---+---+---+---+\n"

-- boardFromFen
-- boardToFen
-- http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation (test cases)

-- naive scoring function


main = do
    putStrLn "Hello, World!"
    putStrLn (showHex (pawns startPosition) "")
    putStrLn (show (pieceAtIndex startPosition 25))
