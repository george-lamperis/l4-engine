#include <stdio.h>
#include <string.h>

#include "l4engine.h"

// TODO 
// - print info about compiled executable somewhere:
//      compiler/version
//      date/time?
// - printf buffering
// - figure out buffer size for strings
// - more info in print_position()

#include <windows.h>


int main ()
{
    HANDLE input_handle = GetStdHandle(STD_INPUT_HANDLE);
    DWORD events = 0;			// how many events took place
    INPUT_RECORD input_record;	// a record of input events
    DWORD input_size = 1;		// how many characters to read
    char buffer[256];

    if (input_handle == INVALID_HANDLE_VALUE) {
        exit(-1);
    }   

    FlushConsoleInputBuffer(input_handle); // prevent from blocking on first iteration
 
    while (true) {
       PeekConsoleInput(input_handle, &input_record, input_size, &events);

        printf("events %d\n", events);
        if (events) {
            printf("blocking\n");
            gets(buffer);

            // if you uncomment this line, pressing [enter] seems 
            // to make this block twice
            FlushConsoleInputBuffer(input_handle);
        }

        Sleep(500);
    }

  	return 0;
}
   
