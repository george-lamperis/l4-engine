#include "l4engine.h"

#include <iostream>
#include <iomanip>
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

//const bitboard_t Square[64];
bitboard_t Square[64];


// Brian Kernighan's way
// Consecutively reset LS1B in a loop body and counting 
// loop cycles until the bitset becomes empty. Brian Kernighan
// mentioned the trick in his and Ritchie's book The 
// C Programming_Language, 2nd Edition 1988, exercise 2-9. 

int popCount (bitboard_t x) 
{
    int count = 0;
    while (x) {
        count++;
        x &= x - 1; // reset LS1B
    }

    return count;
}


bitboard_t rank_mask(size_t rank)
{
    assert(rank <= 8);
    return Rank[rank];
}

bitboard_t square_mask(size_t sq)
{
    // assert nonnegative?
    assert(sq <= 63);
    return Square[sq];
}


void print_bitboard(bitboard_t b) 
{
    cout << "\t a b c d e f g h " << endl;
    cout << "\t+---------------+" << endl;

    for (int i = 56; i >= 0; i -= 8) {
        cout << "\t";

        for (int j = 0; j < 8; j++ ) {
            int bit = b & square_mask(i+j);
            cout << setw(2) << bit << "|";
        }
    }
       
    for (int i = RANK_8; i >= RANK_1; i--) {

    }

    cout << "\t+---------------+" << endl;
    cout << "\t a b c d e f g h " << endl;
}
