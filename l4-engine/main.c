#include <stdio.h>

#include "l4engine.h"

// TODO 
// - print info about compiled executable somewhere:
//      compiler/version
//      date/time?
// - figure out uint64/inttypes.h for windows


int main ()
{
    
    // setvbuf(stream, NULL, _IONBF, 0);
    
    bitboard_t b = square_mask(H8) | square_mask(G8);

    printf("%" PRIx64 "\n", b);
    print_bitboard(b);

    // uci_input();

	return 0;
}
   
