#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <Windows.h>

#include "l4engine.h"


enum eEngineState engine_state = IDLE;
struct move bestmove = { 0 };


void uci_loop()
{
    //if (DEBUGGING) {
    //    FILE *log = fopen("logfile.txt", "w");
    //}

    char buffer[BUFFER_SIZE];

    while (true) {
        gets(buffer);       // block here waiting for input

        if (strcmp(buffer, "quit") == 0) {
            printf("exiting...\n");
            fflush(stdout);
            break;
        } else if (strcmp(buffer, "uci") == 0) {
            printf("id name l4-engine\n");
            printf("id author George Lamperis\n");
            printf("uciok\n");
            fflush(stdout);
        } else if (strcmp(buffer, "ucinewgame") == 0) {
            // do nothing
        } else if (strcmp(buffer, "isready") == 0) {
            printf("readyok\n");
            fflush(stdout);
        } else if (strstr(buffer, "position") != NULL) {

        } else if (strstr(buffer, "go") != NULL) {

        } else if (strcmp(buffer, "stop") == 0) {

        } else {
            printf("unrecongnized command\n");
            fflush(stdout);
        }
    }


}


void perft(int n)
{

}
