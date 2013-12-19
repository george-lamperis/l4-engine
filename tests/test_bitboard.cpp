#include <gtest/gtest.h>
#include "l4engine.h"


TEST(AllWhiteTest, onlytest) {
    bitboard white = all_white(startpos);
    EXPECT_EQ(UINT64_C(0x000000000000FFFF), white);
}

TEST(AllBlackTest, onlytest) {
    bitboard black = all_black(startpos);
    EXPECT_EQ(UINT64_C(0xFFFF000000000000), black);
}

TEST(AllPiecesTest, onlytest) {
    bitboard all = all_pieces(startpos);
    EXPECT_EQ(UINT64_C(0xFFFF00000000FFFF), all);
}


TEST(ComparePositionTest, Equal) {
    EXPECT_TRUE(positions_equal(startpos, startpos));
}

TEST(TestComparePosition, NotEqual) {
    struct position pos = startpos;
    pos.b_queens = 0;
    EXPECT_FALSE(positions_equal(pos, startpos));
}
