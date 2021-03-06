#include <stdio.h>
#include <assert.h>

#include "l4engine.h"


const struct position startpos = {
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


bitboard rank_mask(enum eRank rank)
{
    assert(rank <= 8);
    return UINT64_C(0x00000000000000FF) << (8 * rank);
}


bitboard file_mask(enum eFile file)
{
    assert(file <= 8);
    return UINT64_C(0x0101010101010101) << file;
}


bitboard square_mask(enum eSquare sq)
{
    assert(sq <= 63);
    return UINT64_C(1) << sq;
}


bitboard all_white(struct position pos)
{
    return (
        pos.w_pawns |
        pos.w_rooks |
        pos.w_knights |
        pos.w_bishops |
        pos.w_queens |
        pos.w_king
    );
}


bitboard all_black(struct position pos)
{
    return (
        pos.b_pawns |
        pos.b_rooks |
        pos.b_knights |
        pos.b_bishops |
        pos.b_queens |
        pos.b_king
    );
}


bitboard all_pieces(struct position pos)
{
    return (
        pos.w_pawns |
        pos.w_rooks |
        pos.w_knights |
        pos.w_bishops |
        pos.w_queens |
        pos.w_king |

        pos.b_pawns |
        pos.b_rooks |
        pos.b_knights |
        pos.b_bishops |
        pos.b_queens |
        pos.b_king
    );
}


bool positions_equal(struct position a, struct position b)
{
    return (
        (a.w_pawns == b.w_pawns) &&
        (a.w_rooks == b.w_rooks) &&
        (a.w_knights == b.w_knights) &&
        (a.w_bishops == b.w_bishops) &&
        (a.w_queens == b.w_queens) &&
        (a.w_king == b.w_king) &&

        (a.b_pawns == b.b_pawns) &&
        (a.b_rooks == b.b_rooks) &&
        (a.b_knights == b.b_knights) &&
        (a.b_bishops == b.b_bishops) &&
        (a.b_queens == b.b_queens) &&
        (a.b_king == b.b_king) &&

        (a.en_passant == b.en_passant) &&
        (a.halfmove == b.halfmove) &&
        (a.fullmove == b.fullmove) &&

        (a.w_kingside == b.w_kingside) &&
        (a.w_queenside == b.w_queenside) &&
        (a.b_kingside == b.b_kingside) &&
        (a.b_queenside == b.b_queenside) &&
        (a.whites_turn == b.whites_turn)
    );
}

// Brian Kernighan's way
// Consecutively reset LS1B in a loop body and counting loop cycles until the
// bitset becomes empty. Brian Kernighan mentioned the trick in his and
// Ritchie's book The C Programming_Language, 2nd Edition 1988, exercise 2-9.
int bit_count (bitboard b)
{
    int count = 0;
    while (b) {
        count++;
        b &= b - 1; // reset LS1B
    }

    return count;
}


void print_bitboard(bitboard b) 
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        printf("\t%d ", rank);

        for (int j = 0; j < 8; j++ ) {
            bitboard bit = (b & square_mask(i+j));

            if (b & bit)
                printf("| X ");
            else
                printf("|   ");
        }

        printf( "| %d\n", rank);
        printf("\t  +---+---+---+---+---+---+---+---+\n");
    }

    printf("\t    a   b   c   d   e   f   g   h  \n");
    fflush(stdout);
}


// TODO
// print move numbers, players turn, etc.
void print_position(struct position p)
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        printf("\t%d ", rank);

        for (int j = 0; j < 8; j++ ) {
            bitboard mask = square_mask(i+j);

            if (p.w_pawns & mask)
                printf("| P ");
            else if (p.w_rooks & mask)
                printf("| R ");
            else if (p.w_knights & mask)
                printf("| N ");
            else if (p.w_bishops & mask)
                printf("| B ");
            else if (p.w_queens & mask)
                printf("| Q ");
            else if (p.w_king & mask)
                printf("| K ");
            else if (p.b_pawns & mask)
                printf("| p ");
            else if (p.b_rooks & mask)
                printf("| r ");
            else if (p.b_knights & mask)
                printf("| n ");
            else if (p.b_bishops & mask)
                printf("| b ");
            else if (p.b_queens & mask)
                printf("| q ");
            else if (p.b_king & mask)
                printf("| k ");
            else
                printf("|   ");
        }

        printf( "| %d\n", rank);
        printf("\t  +---+---+---+---+---+---+---+---+\n");
    }

    printf("\t    a   b   c   d   e   f   g   h  \n");
    fflush(stdout);
}
