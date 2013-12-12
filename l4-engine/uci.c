#include <stdio.h>
#include "l4engine.h"


void engine_loop()
{

}


void uci_identify()
{
    printf("id name l4-engine\n");
    printf("id author George Lamperis\n");
    printf("uciok\n");
    fflush(stdout);
}


void uci_readyok()
{
    printf("readyok\n");
    fflush(stdout);
}


void uci_input()
{
    char buffer[256];
}


struct chessboard_t parse_fen(const char *fen)
{
    struct chessboard_t b = {
        .w_pawns = 1,
    };

    char *pieces;
    char *color;
    char *castling;
    char *enpassant;
    char *halfmove;
    char *fullmove;

    // TODO assert the string makes sense

    // Figure out bitboards
    for (int i = 56; i >= 0; i -= 8) {
        int rank = (i / 8) + 1;

        for (int j = 0; j < 8; j++ ) {
            bitboard_t bit = square_mask(i+j);

        }
    }

    return b;
}
