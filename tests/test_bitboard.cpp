#include <gtest/gtest.h>
#include "l4engine.h"

TEST(ComparePositionTest, Equal) {
    EXPECT_TRUE(positions_equal(startpos, startpos));
}

TEST(TestComparePosition, NotEqual) {
    struct position pos = startpos;
    // pos.b_queens = 0;
    EXPECT_FALSE(positions_equal(pos, startpos));
}
