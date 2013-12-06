#ifndef L4_ENGINE_H
#define L4_ENGINE_H

#include <stdint.h>

// -----------------------------------------------------------------------------
// bitboard.cpp
// Bitboards and lookup tables and the chessboard data
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

// TODO unsigned?
typedef int64_t bitboard_t;

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


// TODO change order to follow FEN?
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

    // at most one bit set. That bit  is the location
    // of the pawn which moved two spaces forward.
    bitboard_t w_enpassant;
    bitboard_t b_enpassant;

    // can castle?
    bool w_kingside;
    bool w_queenside;
    bool b_kingside;
    bool b_queenside;

    // whose turn?
    bool whites_turn;
};

extern const struct chessboard_t chess_initial_state;

// These functions defined in bitboard.cpp
bitboard_t rank_mask(size_t rank);
bitboard_t file_mask(size_t file);
bitboard_t square_mask(size_t sq);

bitboard_t all_white(chessboard_t b);
bitboard_t all_black(chessboard_t b);

chessboard_t move(eSquare src, eSquare dst, chessboard_t board);

void print_bitboard(bitboard_t b);
void print_chessboard(chessboard_t board);

// -----------------------------------------------------------------------------
// search.cpp
// contains code which generates all legal moves
// -----------------------------------------------------------------------------

int bit_count(bitboard_t b);

void search_pawns(const chessboard_t);
void search_rooks(const chessboard_t);
void search_knights(const chessboard_t);
void search_bishops(const chessboard_t);
void search_queens(const chessboard_t);
void search_king(const chessboard_t);
void search_moves(const chessboard_t);

// -----------------------------------------------------------------------------
// evaluate.cpp
// contains logic to assign a score to a chess state.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// uci.cpp
// contains functions for interfacing with UCI
// -----------------------------------------------------------------------------

enum eEngineState {
    IDLE, 
    THINKING,
    PONDERING, 
};


chessboard_t parse_fen(const char *fen);
chessboard_t parse_pos(const char *pos);

void uci_identify();
void uci_option();
void uci_readyok();

// uci_info(int depth, score, ...);

#endif // L4_ENGINE_H
