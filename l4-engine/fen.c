#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "l4engine.h"


// Helper function
void parse_pieces(char *pieces, struct position *pos)
{
    int i = 0;
    int length = strlen(pieces);
    int square_counter = 0;

    while (i < length) {
        char c = pieces[i];
        i++;

        if (isdigit(c)) {
            int offset = c - '0';
            square_counter += offset;
            continue;
        } else if (c == '/') {
            continue;
        }

        int rank = 7 - (square_counter / 8);
        int file = square_counter % 8;
        bitboard mask = square_mask(8 * rank + file);
        square_counter++;

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
        default:
            exit(EXIT_FAILURE);
        } // end switch
    } // end while()

    assert(square_counter == 64);
}


void parse_color(char *color, struct position *pos)
{
    assert(strlen(color) == 1);

    if (color[0] == 'w')
        pos->whites_turn = true;
    else
        pos->whites_turn = false;
}


void parse_castling(char *castling, struct position *pos)
{
    // make sure all initialized to false
    pos->w_kingside = false;
    pos->w_queenside = false;
    pos->b_kingside = false;
    pos->b_queenside = false;

    int length = strlen(castling);
    for (int i = 0; i < length; i++) {
        switch (castling[i]) {
        case WHITE_KING:
            pos->w_kingside = true;
            break;
        case WHITE_QUEEN:
            pos->w_queenside = true;
            break;
        case BLACK_KING:
            pos->b_kingside = true;
            break;
        case BLACK_QUEEN:
            pos->b_queenside = true;
            break;
        default:
            break;
        }
    }
}


void parse_enpassant(char *ep, struct position *pos)
{
    if (strcmp(ep, "-") == 0) {
        pos->en_passant = 0;
    } else {
        int file = ep[0] - 'a';
        int rank = ep[1] - '1';

        pos->en_passant = square_mask(8 * rank + file);
    }
}


/*
* parses the input of UCI's position command, returns a struct containing
* that position.
*/
struct position parse_position(const char *pos_str)
{
    assert(strlen(pos_str) < BUFFER_SIZE);

    char buffer[BUFFER_SIZE];
    struct position pos = { 0 };
    
    strcpy(buffer, pos_str);            // make a copy, strtok() is picky
    strtok(buffer, " \t\n");            // discard "position"
    char *t = strtok(NULL, " \t\n");    // piece configuration

    if (strcmp(t, "startpos") == 0) {
        pos = startpos;
    } else {
        parse_pieces(t, &pos);

        t = strtok(NULL, " \t\n");  // active color
        parse_color(t, &pos);

        t = strtok(NULL, " \t\n");  // castling
        parse_castling(t, &pos);

        t = strtok(NULL, " \t\n");  // en passant
        parse_enpassant(t, &pos);

        t = strtok(NULL, " \t\n");  // half move
        pos.halfmove = atoi(t);

        t = strtok(NULL, " \t\n");  // full move
        pos.fullmove = atoi(t);
    }

    t = strtok(NULL, " \t\n");      // "moves" or NULL
    if (t != NULL) {
        // parse moves
    }

    return pos;
}
