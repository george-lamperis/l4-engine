#include <gtest/gtest.h>
#include "l4engine.h"

TEST(compare_chessboard, simpletest) {
    EXPECT_TRUE(compare_chessboards(chess_initial_state, chess_initial_state));
}
