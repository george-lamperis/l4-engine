#include <stdio.h>
#include <assert.h>

#include "l4engine.h"


const struct chessboard_t chess_initial_state = {
    .w_pawns   = UINT64_C(0x000000000000FF00),
    .w_rooks   = UINT64_C(0x0000000000000081),
    .w_knights = UINT64_C(0x0000000000000042),
    .w_bishops = UINT64_C(0x0000000000000024),
    .w_queens  = UINT64_C(0x0000000000000008),
    .w_king    = UINT64_C(0x0000000000000010),

    .b_pawns   = UINT64_C(0x00FF000000000000),
    .b_rooks   = UINT64_C(0x8100000000000000),
    .b_knights = UINT64_C(0x4200000000000000),
    .b_bishops = UINT64_C(0x2400000000000000),
    .b_queens  = UINT64_C(0x0800000000000000),
    .b_king    = UINT64_C(0x1000000000000000),

    .en_passant = 0,
    .halfmove = 0,
    .fullmove = 1,
    .w_kingside = true,
    .w_queenside = true,
    .b_kingside = true,
    .b_queenside = true,
    .whites_turn = true
};


bitboard_t rank_mask(enum eRank rank)
{
    assert(rank <= 8);
    return UINT64_C(0x00000000000000FF) << (8 * rank);
}


bitboard_t file_mask(enum eFile file)
{
    assert(file <= 8);
    return UINT64_C(0x0101010101010101) << file;
}


bitboard_t square_mask(enum eSquare sq)
{
    assert(sq <= 63);
    return UINT64_C(1) << sq;
}


bool compare_chessboards(struct chessboard_t a, struct chessboard_t b)
{
    bool b1 = a.w_pawns == b.w_pawns;

    return false;
}

// Brian Kernighan's way
// Consecutively reset LS1B in a loop body and counting loop cycles until the
// bitset becomes empty. Brian Kernighan mentioned the trick in his and
// Ritchie's book The C Programming_Language, 2nd Edition 1988, exercise 2-9.
int bit_count (bitboard_t x)
{
    int count = 0;
    while (x) {
        count++;
        x &= x - 1; // reset LS1B
    }

    return count;
}


void print_bitboard(bitboard_t b) 
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        printf("\t%d ", rank);

        for (int j = 0; j < 8; j++ ) {
            bitboard_t bit = (b & square_mask(i+j));

            if (b & bit)
                printf("| X ");
            else
                printf("|   ");
        }

        printf( "| %d\n", rank);
        printf("\t  +---+---+---+---+---+---+---+---+\n");
    }

    printf("\t    a   b   c   d   e   f   g   h  \n");
}


// TODO
// print move numbers, players turn, etc.
void print_chessboard(struct chessboard_t board)
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        printf("\t%d ", rank);

        for (int j = 0; j < 8; j++ ) {
            bitboard_t mask = square_mask(i+j);

            if (board.w_pawns & mask)
                printf("| P ");
            else if (board.w_rooks & mask)
                printf("| R ");
            else if (board.w_knights & mask)
                printf("| N ");
            else if (board.w_bishops & mask)
                printf("| B ");
            else if (board.w_queens & mask)
                printf("| Q ");
            else if (board.w_king & mask)
                printf("| K ");
            else if (board.b_pawns & mask)
                printf("| p ");
            else if (board.b_rooks & mask)
                printf("| r ");
            else if (board.b_knights & mask)
                printf("| n ");
            else if (board.b_bishops & mask)
                printf("| b ");
            else if (board.b_queens & mask)
                printf("| q ");
            else if (board.b_king & mask)
                printf("| k ");
            else
                printf("|   ");
        }

        printf( "| %d\n", rank);
        printf("\t  +---+---+---+---+---+---+---+---+\n");
    }

    printf("\t    a   b   c   d   e   f   g   h  \n");
}
