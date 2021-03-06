module L4Engine where

import Data.Word (Word64)
import Data.Bits
import Data.List
import Data.List.Split
import Data.Char
import Numeric (showHex)

type Bitboard = Word64

data Position = Position { pawns :: Bitboard
                         , rooks :: Bitboard
                         , knights :: Bitboard
                         , bishops :: Bitboard
                         , queens :: Bitboard
                         , kings :: Bitboard
                         , black :: Bitboard
                         , white :: Bitboard
                         , enPassant :: Bitboard
                         , whiteKingside :: Bool
                         , whiteQueenside :: Bool
                         , blackKingside :: Bool
                         , blackQueenside :: Bool
                         , whitesTurn :: Bool
                         , halfMove :: Int
                         , fullMove :: Int
                         } deriving (Show, Eq)

data Piece = BlackPawn | BlackRook | BlackKnight | BlackBishop | BlackQueen |
             BlackKing | WhitePawn | WhiteRook | WhiteKnight | WhiteBishop |
             WhiteQueen | WhiteKing | NotOccupied deriving (Show)

startPosition :: Position
startPosition = Position {pawns     = 0x00ff00000000ff00,
                          rooks     = 0x8100000000000081,
                          knights   = 0x4200000000000042,
                          bishops   = 0x2400000000000024,
                          queens    = 0x0800000000000008,
                          kings     = 0x1000000000000010,
                          black     = 0xffff000000000000,
                          white     = 0x000000000000ffff,
                          enPassant = 0x0000000000000000,
                          whiteKingside  = True,
                          whiteQueenside = True,
                          blackKingside  = True,
                          blackQueenside = True,
                          whitesTurn = True,
                          halfMove = 0,
                          fullMove = 1
                         }

blackPawns :: Position -> Bitboard
blackPawns pos = (pawns pos) .&. (black pos)

blackRooks :: Position -> Bitboard
blackRooks pos = (rooks pos) .&. (black pos)

blackKnights :: Position -> Bitboard
blackKnights pos = (knights pos) .&. (black pos)

blackBishops :: Position -> Bitboard
blackBishops pos = (bishops pos) .&. (black pos)

blackQueens :: Position -> Bitboard
blackQueens pos = (queens pos) .&. (black pos)

blackKing :: Position -> Bitboard
blackKing pos = (kings pos) .&. (black pos)

whitePawns :: Position -> Bitboard
whitePawns pos = (pawns pos) .&. (white pos)

whiteRooks :: Position -> Bitboard
whiteRooks pos = (rooks pos) .&. (white pos)

whiteKnights :: Position -> Bitboard
whiteKnights pos = (knights pos) .&. (white pos)

whiteBishops :: Position -> Bitboard
whiteBishops pos = (bishops pos) .&. (white pos)

whiteQueens :: Position -> Bitboard
whiteQueens pos = (queens pos) .&. (white pos)

whiteKing :: Position -> Bitboard
whiteKing pos = (kings pos) .&. (white pos)

allPieces :: Position -> Bitboard
allPieces pos = (white pos) .|. (black pos)

pieceAtIndex :: Position -> Int -> Piece
pieceAtIndex pos idx
    | testBit (blackPawns pos) idx = BlackPawn
    | testBit (blackRooks pos) idx = BlackRook
    | testBit (blackKnights pos) idx = BlackKnight
    | testBit (blackBishops pos) idx = BlackBishop
    | testBit (blackQueens pos) idx = BlackQueen
    | testBit (blackKing pos) idx = BlackKing
    | testBit (whitePawns pos) idx = WhitePawn
    | testBit (whiteRooks pos) idx = WhiteRook
    | testBit (whiteKnights pos) idx = WhiteKnight
    | testBit (whiteBishops pos) idx = WhiteBishop
    | testBit (whiteQueens pos) idx = WhiteQueen
    | testBit (whiteKing pos) idx = WhiteKing
    | otherwise = NotOccupied

-- returns a String "label" of length 1 for the given piece
pieceLabel :: Piece -> String
pieceLabel BlackPawn   = "p"
pieceLabel BlackRook   = "r"
pieceLabel BlackKnight = "n"
pieceLabel BlackBishop = "b"
pieceLabel BlackQueen  = "q"
pieceLabel BlackKing   = "k"
pieceLabel WhitePawn   = "P"
pieceLabel WhiteRook   = "R"
pieceLabel WhiteKnight = "N"
pieceLabel WhiteBishop = "B"
pieceLabel WhiteQueen  = "Q"
pieceLabel WhiteKing   = "K"
pieceLabel NotOccupied = " "

-- returns an array of piece labels in order from a8 to h8, a7 to h7, and so on.
pieceLabels :: Position -> [String]
pieceLabels pos =
    let pieces = map (pieceAtIndex pos) [8*x + y | x <- [7,6..0], y <- [0..7]]
    in  map pieceLabel pieces

-- Returns an ASCII chessboard graphic of the position.
prettyShow :: Position -> String
prettyShow pos =
    let fileLabels = "\t    a   b   c   d   e   f   g   h  \n"
        divider =    "\t  +---+---+---+---+---+---+---+---+\n"
        rankLabels = [show n | n <- [8,7..1]]
        ranks = map (intercalate " | ") (chunksOf 8 (pieceLabels pos))
    in  fileLabels ++
        divider ++
        intercalate divider (zipWith (\x y -> concat ["\t", x, " | ", y, " | ", x, "\n"]) rankLabels ranks) ++
        divider ++
        fileLabels
        -- castling
        -- enPassant
        -- move counters and active color

-- TODO
-- naive scoring function
-- compile with -Wall?
-- remove unnecessary imports/exports
-- move library source to lib/
-- test suite organization?
-- Bit trick in enPassantSquare: make it safe. In a function?

spacesToDigits :: String -> String
spacesToDigits s =
    concat [if (head x) == ' ' then [intToDigit (length x)] else x | x <- group s]

piecePlacement :: Position -> String
piecePlacement pos =
    let ranks = [concat x | x <- (chunksOf 8 (pieceLabels pos))]
    in  intercalate "/" (map spacesToDigits ranks)

activeColor :: Position -> String
activeColor pos
    | whitesTurn pos = "w"
    | otherwise      = "b"

castling :: Position -> String
castling pos
    | all (== "") [wKing, wQueen, bKing, bQueen] = "-"
    | otherwise = wKing ++ wQueen ++ bKing ++ bQueen
    where wKing = if whiteKingside pos then "K" else ""
          wQueen = if whiteQueenside pos then "Q" else ""
          bKing = if blackKingside pos then "k" else ""
          bQueen = if blackQueenside pos then "q" else ""

indexToSquare :: Int -> String
indexToSquare idx =
    let rank = show ((idx `div` 8) + 1)
        file = chr (idx `mod` 8 + ord 'a') :[]
    in  file ++ rank

enPassantSquare :: Position -> String
enPassantSquare pos
    | enPassant pos == 0 = "-"
    | otherwise = indexToSquare (popCount (enPassant pos - 1))  -- idx of set bit

positionToFen :: Position -> String
positionToFen pos = (piecePlacement pos) ++ " "
                 ++ (activeColor pos) ++ " "
                 ++ (castling pos) ++ " "
                 ++ (enPassantSquare pos) ++ " "
                 ++ show (halfMove pos) ++ " "
                 ++ show (fullMove pos)

fenToPosition :: String -> Position
fenToPosition fen = startPosition
