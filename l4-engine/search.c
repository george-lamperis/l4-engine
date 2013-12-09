#include "l4engine.h"

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


chessboard_t move(eSquare start, eSquare end, chessboard_t board)
{

    return board;
}
