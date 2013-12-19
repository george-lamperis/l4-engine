#include <gtest/gtest.h>

#include "l4engine.h"

TEST(ParsePositionTest, CompactStartpos) {
    struct position actual = parse_position("position startpos");
    EXPECT_TRUE(positions_equal(startpos, actual));
}

TEST(ParsePositionTest, LongStartpos) {
    const char *case1 = "position rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1\n";
    struct position actual = parse_position(case1);
    EXPECT_TRUE(positions_equal(startpos, actual));
}

TEST(ParsePositionTest, moves) {
    const char *case2 = "position startpos moves b2b3\n";
}
