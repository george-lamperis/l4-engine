#include <stdio.h>
#include <inttypes.h>

#include "l4engine.h"

int main ()
{
//    setvbuf(stream, NULL, _IONBF, 0);
    
    bitboard_t b = square_mask(H8);
    print_bitboard(b);
    printf("%d\n", bit_count(square_mask(A1)));
//    uci_input();

	return 0;
}
   
