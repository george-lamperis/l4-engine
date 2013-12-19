#include <gtest/gtest.h>

#include "l4engine.h"

TEST(ParsePositionTest, startpos) 
{
    const char *initial = "position rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1\n";
    struct position actual = parse_position(initial);
    EXPECT_TRUE( positions_equal(startpos, actual) );

    const char *case2 = "position startpos moves b2b3\n";
    parse_position(case2);
}
