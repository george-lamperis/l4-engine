#include <iostream>
#include <string> 

#include "l4engine.h"

using namespace std;

enum {STATUS_PONDERING, STATUS_IDLE, STATUS_QUIT};

void uci_identify()
{
    cout << "id name l4-engine" << endl;
    cout << "id author George Lamperis" << endl;
    cout << "uciok" << endl;
}


void uci_readyok()
{
    cout << "readyok" << endl;
}


void uci_input()
{
    string Line; //to read the command given by the GUI
	int flag =1; //to change the value of chess squares from 'a' to 'h'

	cout.setf (ios::unitbuf);// Make sure that the outputs are sent straight away to the GUI

	while( getline( cin, Line ) ) {
		if ( Line == "uci" ) {
            uci_identify();
		} else if ( Line == "quit" ) {
			cout << "Bye Bye" << endl;
			break;
		} else if ( Line == "isready" ) {
            uci_readyok();
		} else if ( Line == "ucinewgame" ) {
			; // nothing to do
		}

		if ( Line.substr(0,23) == "position startpos moves " ) {
			; // nothing to do
		} else if ( Line == "stop" ) {
			; // nothing to do
		} else if ( Line.substr( 0, 3 ) == "go " ) {
			// Received a command like: "go wtime 300000 btime 300000 winc 0 binc 0"
			cout << "bestmove " << char(105-flag) << "7" << char(105-flag) << "5" << endl;
			//Output like: "bestmove h7h5"
			flag++; //increase flag to move other pawn on next turn
		}
	}

}
