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
struct chessboard_t chessboard_initial_state;

// -----------------------------------------------------------------------------
// Bitboards and lookup tables
// -----------------------------------------------------------------------------



#endif // L4_ENGINE_H