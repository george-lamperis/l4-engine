#include <gtest/gtest.h>
#include "l4engine.h"

TEST(ComparePositionTest, Equal) {
    EXPECT_TRUE(positions_equal(startpos, startpos));
}

TEST(TestComparePosition, NotEqual) {
    struct position pos = startpos;
    pos.b_queens = 0;
    EXPECT_FALSE(positions_equal(pos, startpos));
}

TEST(AllWhiteTest, onlytest) {
    bitboard white = all_white(startpos);
    print_bitboard(white);
    EXPECT_EQ(UINT64_C(0x000000000000FFFF), white);
}

TEST(AllBlackTest, onlytest) {

}

TEST(AllPiecesTest, onlytest) {

}
