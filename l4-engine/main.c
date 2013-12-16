#include <stdio.h>

#include "l4engine.h"

// TODO 
// - print info about compiled executable somewhere:
//      compiler/version
//      date/time?
// - figure out uint64/inttypes.h for windows
// - printf buffering

// CriticalSection for threading
// compareandexchange

int main ()
{
    // setvbuf(stream, NULL, _IONBF, 0);
    
    uci_input();
    
	return 0;
}
   
