#ifndef L4_ENGINE_H
#define L4_ENGINE_H

#include <stdint.h>

// These variables should only be read.
// only modified in uci.cpp
extern int uci_status;
extern char best_move[5];
extern int best_score;

void create_engine_thread();
unsigned __stdcall engine_loop(void* pArguments);

// -----------------------------------------------------------------------------
// chess data
// -----------------------------------------------------------------------------
typedef int64_t bitboard_t;

struct chessboard_t {
    bitboard_t w_pawns;
    bitboard_t w_rooks;
    bitboard_t w_knights;
    bitboard_t w_bishops;    
    bitboard_t w_queens;
    bitboard_t w_king;

    bitboard_t b_pawns;
    bitboard_t b_rooks;
    bitboard_t b_knights;
    bitboard_t b_bishops;
    bitboard_t b_queens;
    bitboard_t b_king;

    // can castle?
    // en passant?
    // whose turn?
    // last move?
};

// const
// struct chessboard_t chessboard_initial_state;

// -----------------------------------------------------------------------------
// Bitboards and lookup tables
// lookup tables are indexed left to right, top to bottom
// -----------------------------------------------------------------------------

// LERF encoding
// inspired/copied from
// http://pages.cs.wisc.edu/~psilord/blog/data/chess-pages/rep.html
// http://chessprogramming.wikispaces.com/Square+Mapping+Considerations
//
//        a    b    c    d    e    f    g    h
//      +----+----+----+----+----+----+----+----+
//    8 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 8
//      +----+----+----+----+----+----+----+----+
//    7 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 7
//      +----+----+----+----+----+----+----+----+
//    6 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 6
//      +----+----+----+----+----+----+----+----+
//    5 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 5
//      +----+----+----+----+----+----+----+----+
//    4 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 4
//      +----+----+----+----+----+----+----+----+
//    3 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 3
//      +----+----+----+----+----+----+----+----+
//    2 | 8  | 9  | 10 | 11 | 12 | 13 | 14 | 15 | 2
//      +----+----+----+----+----+----+----+----+
//    1 | 0  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 1
//      +----+----+----+----+----+----+----+----+
//        a    b    c    d    e    f    g    h


// ANDing with these results in a bitboard
// with only the bits in that rank/file set.
extern const bitboard_t Rank[8];
extern const bitboard_t File[8];
extern const bitboard_t Square[8];

#endif // L4_ENGINE_H
