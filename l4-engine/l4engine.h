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

// TODO unsigned?
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
// implementation in bitboard.cpp
// -----------------------------------------------------------------------------
//
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
//
//  Neighbors compass rose
//      +----+----+----+
//      | +7 | +8 | +9 | 
//      +----+----+----+
//      | -1 |  0 | +1 | 
//      +----+----+----+
//      | -9 | -8 | -7 | 
//      +----+----+----+
// -----------------------------------------------------------------------------


enum eRank { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8 };
enum eFile { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H };

enum eSquare {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
};

// abstract the arrays away
bitboard_t rank_mask(size_t rank);
bitboard_t file_mask(size_t file);
bitboard_t square_mask(size_t sq);

void print_bitboard(bitboard_t b);
// ANDing with these results in a bitboard
// with only the bits in that rank/file set.

#endif // L4_ENGINE_H
