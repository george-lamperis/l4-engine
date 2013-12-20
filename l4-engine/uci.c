#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

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


// Helper function
void parse_pieces(char *pieces, struct position *pos)
{
    int i = 0;
    int length = strlen(pieces);
    int square_counter = 0;

    while (i < length) {
        char c = pieces[i];
        i++;
    
        if ( isdigit(c) ) {
            int offset = c - '0';
            square_counter += offset;
            continue;
        } else if ( isspace(c) || (c == '/') ){
            continue;
        }

        int rank = 7 - (square_counter / 8);
        int file = square_counter % 8;
        bitboard mask = square_mask(8 * rank + file);
        square_counter++;

        printf("%d\n", square_counter);

        switch (c) {
        case WHITE_PAWN:
            pos->w_pawns |= mask;
            break;
        case WHITE_ROOK:
            pos->w_rooks |= mask;
            break;
        case WHITE_KNIGHT:
            pos->w_knights |= mask;
            break;
        case WHITE_BISHOP:
            pos->w_bishops |= mask;
            break;
        case WHITE_QUEEN:
            pos->w_queens |= mask;
            break;
        case WHITE_KING:
            pos->w_king |= mask;
            break;
        case BLACK_PAWN:
            pos->b_pawns |= mask;
            break;
        case BLACK_ROOK:
            pos->b_rooks |= mask;
            break;
        case BLACK_KNIGHT:
            pos->b_knights |= mask;
            break;
        case BLACK_BISHOP:
            pos->b_bishops |= mask;
            break;
        case BLACK_QUEEN:
            pos->b_queens |= mask;
            break;
        case BLACK_KING:
            pos->b_king |= mask;
            break;
        //default:
            //exit(EXIT_FAILURE);
        }

    } // end while()

    assert(square_counter == 64);
}


/*
 * parses the input of UCI's position command, returns a struct containing
 * that position.
 */
struct position parse_position(const char *pos_str)
{
    assert(strlen(pos_str) < BUFFER_SIZE);

    // make a copy, strtok() is picky
    char buffer[BUFFER_SIZE];
    strcpy(buffer, pos_str);  

    struct position pos = { 0 };
    int count = 0;
    char *t = strtok(buffer, " \t\n");

    while (t != NULL) {
        switch (count) {
        case 0:     // "position"
            break;
        case 1:     // piece configuration
            parse_pieces(t, &pos);
            break;
        default:
            // exit?
            break;
        }

        t = strtok(NULL, " \t\n");
        count++;
    }

    pos.en_passant = 1;
    return pos;
}

void perft(int n)
{

}
