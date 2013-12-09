#include <gtest/gtest.h>

#include "l4engine.h"

TEST(parse_fen_test, initial_state) 
{
    const char *initial = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    chessboard_t board = parse_fen(initial);

   // EXPECT_TRUE(board == chess_initial_state);

}