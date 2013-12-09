#include <stdio.h>
#include <assert.h>

#include "l4engine.h"

/*
const struct chessboard_t chess_initial_state = {
    rank_mask(RANK_2),                  // w_pawns
    square_mask(A1) | square_mask(H1),  // w_rooks
    square_mask(B1) | square_mask(G1),  // w_knights
    square_mask(C1) | square_mask(F1),  // w_bishops
    square_mask(D1),                    // w_queens
    square_mask(E1),                    // w_king

    rank_mask(RANK_7),                  // b_pawns
    square_mask(A8) | square_mask(H8),  // b_rooks
    square_mask(B8) | square_mask(G8),  // b_knights
    square_mask(C8) | square_mask(F8),  // b_bishops
    square_mask(D8),                    // b_queens
    square_mask(E8),                    // b_king

    0,                                  // en_passant

    0,                                  // halfmove
    1,                                  // fullmove

    true,                               // w_kingside
    true,                               // w_queenside
    true,                               // b_kingside
    true,                               // b_queenside

    true,                               // whites_turn
};
*/

bitboard_t rank_mask(enum eRank rank)
{
    assert(rank <= 8);
    return UINT64_C(0x00000000000000FF) << rank;
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


void print_bitboard(bitboard_t b) 
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        printf("\t%d ", rank);

        for (int j = 0; j < 8; j++ ) {
            bitboard_t bit = b & square_mask(i+j);
            assert(bit_count(bit) == 1);

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


void print_chessboard(chessboard_t board)
{
    printf("\t    a   b   c   d   e   f   g   h  \n");
    printf("\t  +---+---+---+---+---+---+---+---+\n");

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        cout << "\t" << rank << " ";

        for (int j = 0; j < 8; j++ ) {
            bitboard_t mask = square_mask(i+j);

            if (board.w_pawns & mask)
                cout << "| P ";
            else if (board.w_rooks & mask)
                cout << "| R ";
            else if (board.w_knights & mask)
                cout << "| N ";
            else if (board.w_bishops & mask)
                cout << "| B ";
            else if (board.w_queens & mask)
                cout << "| Q ";
            else if (board.w_king & mask)
                cout << "| K ";
            else if (board.b_pawns & mask)
                cout << "| p ";
            else if (board.b_rooks & mask)
                cout << "| r ";
            else if (board.b_knights & mask)
                cout << "| n ";
            else if (board.b_bishops & mask)
                cout << "| b ";
            else if (board.b_queens & mask)
                cout << "| q ";
            else if (board.b_king & mask)
                cout << "| k ";
            else
                cout << "|   ";
        }

        cout << "| " << rank << endl;
        cout << "\t  +---+---+---+---+---+---+---+---+" << endl;
    }

    printf("\t    a   b   c   d   e   f   g   h  \n");
}
