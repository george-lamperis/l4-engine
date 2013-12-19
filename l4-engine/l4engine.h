#ifndef L4_ENGINE_H
#define L4_ENGINE_H

#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>    // TODO what if __cplusplus

#ifdef __cplusplus
extern "C" {
#endif


typedef uint64_t bitboard;

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


struct position {
    bitboard w_pawns;
    bitboard w_rooks;
    bitboard w_knights;
    bitboard w_bishops;
    bitboard w_queens;
    bitboard w_king;

    bitboard b_pawns;
    bitboard b_rooks;
    bitboard b_knights;
    bitboard b_bishops;
    bitboard b_queens;
    bitboard b_king;

    bitboard en_passant;

    int halfmove;
    int fullmove;

    // castling
    bool w_kingside;
    bool w_queenside;
    bool b_kingside;
    bool b_queenside;

    bool whites_turn;
};

struct move {
    enum eSquare from;
    enum eSquare to;
    bool capture;
    bool promotion;
};

// -----------------------------------------------------------------------------
// bitboard.c
// stuff involving the bitboard encoding
// -----------------------------------------------------------------------------
extern const struct position startpos;

bitboard rank_mask(enum eRank rank);
bitboard file_mask(enum eFile file);
bitboard square_mask(enum eSquare sq);

bitboard all_white(struct position pos);
bitboard all_black(struct position pos);
bitboard all_pieces(struct position pos);
bool positions_equal(struct position a, struct position b);

int bit_count(bitboard b);
void print_bitboard(bitboard b);
void print_position(struct position pos);
// TODO something to assert that each set is disjoint

// -----------------------------------------------------------------------------
// move.c 
// move generation stuff
// -----------------------------------------------------------------------------
struct position make_move(struct move, struct position pos);

void generate_moves(struct position pos /*, <list of moves object> */);
bool move_legal(struct move mv, struct position pos);
bool position_legal(struct position pos);

// -----------------------------------------------------------------------------
// uci.c
// contains functions for interfacing with UCI 
// and controlling program execution.
// -----------------------------------------------------------------------------

#define WHITE_PAWN      'W'
#define WHITE_ROOK      'R'
#define WHITE_KNIGHT    'N'
#define WHITE_BISHOP    'B'
#define WHITE_QUEEN     'Q'
#define WHITE_KING      'K'
#define BLACK_PAWN      'w'
#define BLACK_ROOK      'r'
#define BLACK_KNIGHT    'n'
#define BLACK_BISHOP    'b'
#define BLACK_QUEEN     'q'
#define BLACK_KING      'k'

enum eEngineState {
    IDLE, 
    THINKING,
    PONDERING, 
};

// QKRBNP for white pieces, lowercase for black
// http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
struct position parse_position(const char *pos);
// to_fen(); ?

void perft(int depth);

void uci_input();
void uci_identify();
void uci_option();
void uci_readyok();

// uci_info(int depth, score, ...);


#ifdef __cplusplus
}
#endif

#endif // L4_ENGINE_H
