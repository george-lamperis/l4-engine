#include <string>
#include <iostream>
#include <thread>

#include "l4engine.h"

#define DEBUG_LOG 1

using namespace std;

void uci_input();

int main ()
{
    chessboard_t board = chess_initial_state;
    print_chessboard(board);
    uci_input();

	return 0;
}
   
