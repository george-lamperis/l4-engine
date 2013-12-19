#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "l4engine.h"

#define BUFFER_SIZE 256

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
    //char buffer[256];
}


struct position parse_pieces(const char *pieces)
{
    struct position pos = { 0 };

    int i = 0;
    int length = strlen(pieces);
    int rank = 7;
    int file = 0;

    while (i < length) {
        enum eSquare sq= (8 * rank) + file;
        bitboard mask = square_mask(sq);

        if (file == 7) {
            file = 0;
            rank--;
        }

        char c = pieces[i];
        i++;

        if (isdigit(c)) {
            ;
        }

        switch (c) {
        case '/':
            break;
        case WHITE_PAWN:
            break;
        case WHITE_ROOK:
            break;
        case WHITE_KNIGHT:
            break;
        case WHITE_BISHOP:
            break;
        case WHITE_QUEEN:
            break;
        case WHITE_KING:
            break;
        case BLACK_PAWN:
            break;
        case BLACK_ROOK:
            break;
        case BLACK_KNIGHT:
            break;
        case BLACK_BISHOP:
            break;
        case BLACK_QUEEN:
            break;
        case BLACK_KING:
            break;
        }

    }

    return pos;
}


struct position parse_position(const char *pos)
{
    char fen[BUFFER_SIZE];
    char moves[BUFFER_SIZE];
    sscanf(pos, "position %s moves %s\n", fen, moves);

    // parse fen
    int length = strlen(fen);
    for (int i = 0; i < length; i++) {
        char c = fen[i];


    }

    return (struct position) { 0 };
}

void perft(int n)
{

}
