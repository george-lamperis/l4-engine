#include <gtest/gtest.h>

#include "l4engine.h"


TEST(TestPawnMoves, WhiteQuietMoves) {
    struct move move_list[100];
    int remaining = 100;
    
    const char s1[] = "position 4k3/8/8/8/p1p5/4p1p1/PPPPPPPP/4K3 w - - 0 1";
    struct position pos1 = parse_position(s1);
    pawn_moves(pos1, move_list, &remaining);
    print_position(pos1);

}

TEST(TestPawnMoves, Captures) {
}

TEST(TestPawnMoves, EnPassant) {
}