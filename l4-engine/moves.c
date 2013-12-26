#include <assert.h>

#include "l4engine.h"


bool move_legal(struct move mv, struct position pos) 
{
    // disjoint piece sets
    // no checkmate
    // no check
    // needs to have a king I guess

    return false;
}


// one square forward
// two squares forward
// diagonal captures
// en passant
// promotions
int pawn_moves(struct position pos, struct move *cursor, int *remaining)
{
    assert(0 < *remaining);

    // TODO use empty instead of all?
    bitboard all = all_pieces(pos);
    bitboard moves = 0;

    if (pos.whites_turn) {
        moves |= (pos.w_pawns << 8) & (~all);
        moves |= (moves << 8) & (~all);
    } else {
        moves |= (pos.b_pawns >> 8) ^ all;
        moves |= (moves >> 8) ^ all;
    }

    // captures
    //if (pos.whites_turn) {
    //    moves |= (pos.w_pawns << 8) & (~all);
    //    moves |= (moves << 8) & (~all);
    //}
    //else {
    //    moves |= (pos.b_pawns >> 8) ^ all;
    //    moves |= (moves >> 8) ^ all;
    //}
}


int king_moves(struct position pos, struct move *cursor, int *remaining)
{
    return 0;
}

int knight_moves(struct position pos, struct move *cursor, int *remaining)
{
    return 0;
}
/*
 * Fills an array with a list of legal moves.
 * 
 * cursor - a pointer to the next empty spot in an array
 * remaining - pointer to an integer which stores the number of remaining
 *             slots of an array. As we fill the array, we will decrement
 *             this value.
 *
 * returns number of moves generated
 */
int generate_moves(struct position pos, struct move *cursor, int *remaining)
{
    assert(0 < *remaining);

    int n_moves;


    return 0;
}
