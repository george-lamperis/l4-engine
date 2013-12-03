#include <gtest/gtest.h>

#include "l4engine.h"

TEST(FactorialTest, HandlesZeroInput) {
  EXPECT_EQ(1, 0);
}

TEST(sample_test_case, sample_test)
{
    EXPECT_EQ(1, 1);

    bitboard_t x = file_mask(FILE_A);
    print_bitboard(x);

    EXPECT_EQ(1, 1);
}

int main(int argc, char** argv) 
{ 
    testing::InitGoogleTest(&argc, argv); 
    RUN_ALL_TESTS(); 
    std::getchar(); // keep console window open until Return keystroke
}
