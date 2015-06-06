//
//  tests_main.h
//  HelloCpp
//
//  Created by Martin on 31.03.14.
//
//

#ifndef HelloCpp_tests_main_h
#define HelloCpp_tests_main_h

#include <gmock.h>

int init_tests(int, char**);
int init_tests(int argc, char** argv) {
    // Add this line to repeat 1000 times, and each time the tests are run in a different order.
    // ::testing::GTEST_FLAG(repeat) = 1000; ::testing::GTEST_FLAG(shuffle) = true;
    
    // Go into debug mode on failure:
    ::testing::GTEST_FLAG(break_on_failure) = true;
    
    // Run only tests that match a pattern:
//    ::testing::GTEST_FLAG(filter) = "*RemovesLowScoringHypotheses*";
    
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

#endif
