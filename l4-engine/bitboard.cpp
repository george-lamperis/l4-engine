#include "l4engine.h"

#include <iostream>
#include <assert.h>

using namespace std;

//a-file             0x0101010101010101
//h-file             0x8080808080808080
//1st rank           0x00000000000000FF
//8th rank           0xFF00000000000000
//a1-h8 diagonal     0x8040201008040201
//h1-a8 antidiagonal 0x0102040810204080
//light squares      0x55AA55AA55AA55AA
//dark squares       0xAA55AA55AA55AA55

const bitboard_t Rank[8] = {
    0x00000000000000FF,     // rank 1
    0x000000000000FF00,
    0x0000000000FF0000,
    0x00000000FF000000,
    0x000000FF00000000,
    0x0000FF0000000000,
    0x00FF000000000000,
    0xFF00000000000000,     // rank 8
};


const bitboard_t File[8] = {
    0x0101010101010101,     // a-file
    0x0202020202020202,
    0x0404040404040404,
    0x0808080808080808,
    0x1010101010101010,
    0x2020202020202020,
    0x4040404040404040,
    0x8080808080808080      //h-file
};


const bitboard_t Square[64] = {
    File[FILE_A] & Rank[RANK_1],
    File[FILE_B] & Rank[RANK_1],
    File[FILE_C] & Rank[RANK_1],
    File[FILE_D] & Rank[RANK_1],
    File[FILE_E] & Rank[RANK_1],
    File[FILE_F] & Rank[RANK_1],
    File[FILE_G] & Rank[RANK_1],
    File[FILE_H] & Rank[RANK_1],
    File[FILE_A] & Rank[RANK_2],
    File[FILE_B] & Rank[RANK_2],
    File[FILE_C] & Rank[RANK_2],
    File[FILE_D] & Rank[RANK_2],
    File[FILE_E] & Rank[RANK_2],
    File[FILE_F] & Rank[RANK_2],
    File[FILE_G] & Rank[RANK_2],
    File[FILE_H] & Rank[RANK_2],
    File[FILE_A] & Rank[RANK_3],
    File[FILE_B] & Rank[RANK_3],
    File[FILE_C] & Rank[RANK_3],
    File[FILE_D] & Rank[RANK_3],
    File[FILE_E] & Rank[RANK_3],
    File[FILE_F] & Rank[RANK_3],
    File[FILE_G] & Rank[RANK_3],
    File[FILE_H] & Rank[RANK_3],
    File[FILE_A] & Rank[RANK_4],
    File[FILE_B] & Rank[RANK_4],
    File[FILE_C] & Rank[RANK_4],
    File[FILE_D] & Rank[RANK_4],
    File[FILE_E] & Rank[RANK_4],
    File[FILE_F] & Rank[RANK_4],
    File[FILE_G] & Rank[RANK_4],
    File[FILE_H] & Rank[RANK_4],
    File[FILE_A] & Rank[RANK_5],
    File[FILE_B] & Rank[RANK_5],
    File[FILE_C] & Rank[RANK_5],
    File[FILE_D] & Rank[RANK_5],
    File[FILE_E] & Rank[RANK_5],
    File[FILE_F] & Rank[RANK_5],
    File[FILE_G] & Rank[RANK_5],
    File[FILE_H] & Rank[RANK_5],
    File[FILE_A] & Rank[RANK_6],
    File[FILE_B] & Rank[RANK_6],
    File[FILE_C] & Rank[RANK_6],
    File[FILE_D] & Rank[RANK_6],
    File[FILE_E] & Rank[RANK_6],
    File[FILE_F] & Rank[RANK_6],
    File[FILE_G] & Rank[RANK_6],
    File[FILE_H] & Rank[RANK_6],
    File[FILE_A] & Rank[RANK_7],
    File[FILE_B] & Rank[RANK_7],
    File[FILE_C] & Rank[RANK_7],
    File[FILE_D] & Rank[RANK_7],
    File[FILE_E] & Rank[RANK_7],
    File[FILE_F] & Rank[RANK_7],
    File[FILE_G] & Rank[RANK_7],
    File[FILE_H] & Rank[RANK_7],
    File[FILE_A] & Rank[RANK_8],
    File[FILE_B] & Rank[RANK_8],
    File[FILE_C] & Rank[RANK_8],
    File[FILE_D] & Rank[RANK_8],
    File[FILE_E] & Rank[RANK_8],
    File[FILE_F] & Rank[RANK_8],
    File[FILE_G] & Rank[RANK_8],
    File[FILE_H] & Rank[RANK_8]
};


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


bitboard_t rank_mask(size_t rank)
{
    assert(rank <= 8);
    return Rank[rank];
}


bitboard_t file_mask(size_t file)
{
    assert(file <= 8);
    return File[file];
}


bitboard_t square_mask(size_t sq)
{
    assert(sq <= 63);
    return Square[sq];
}


void print_bitboard(bitboard_t b) 
{
    cout << "\t    a   b   c   d   e   f   g   h  " << endl;
    cout << "\t  +---+---+---+---+---+---+---+---+" << endl;

    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;
        cout << "\t" << rank << " ";

        for (int j = 0; j < 8; j++ ) {
            bitboard_t bit = b & square_mask(i+j);

            if (bit)
                cout << "| X ";
            else
                cout << "|   ";
        }

        cout << "| " << rank << endl;
        cout << "\t  +---+---+---+---+---+---+---+---+" << endl;
    }

    cout << "\t    a   b   c   d   e   f   g   h  " << endl;
}


void print_chessboard(chessboard_t board)
{
    cout << "\t    a   b   c   d   e   f   g   h  " << endl;
    cout << "\t  +---+---+---+---+---+---+---+---+" << endl;

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

    cout << "\t    a   b   c   d   e   f   g   h  " << endl;
}

