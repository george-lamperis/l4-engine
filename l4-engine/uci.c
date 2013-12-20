#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "l4engine.h"


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


void perft(int n)
{

}
