//
//  qmc_Tests.cpp
//  qmc
//
//  Created by Martin on 06.06.15.
//  Copyright (c) 2015 Martin Finke. All rights reserved.
//

#include <gtest.h>
#include <gmock.h>

#include "qmc_priv.h"

using ::testing::_;

auto test = [](std::vector<BitVector> cubes, std::vector<QmTerm> expected) {
    auto primes = computePrimes(cubes);
    std::unordered_set<QmTerm, QmTermHash> _expected(expected.begin(), expected.end());
    
    EXPECT_EQ(primes, _expected);
};
auto term = [](int _term, int _mask) {QmTerm term = {.term=_term, .mask=_mask}; return term;};

TEST(computePrimes, BehavesLikeThePythonVersion) {
    test({}, {});
    test({0}, {term(0,0)});
    test({1}, {term(1,0)});
    test({2}, {term(2,0)});
    
    test({3}, {term(3, 0)});
    test({0,1}, {term(0, 1)});
    test({1,2}, {term(2, 0), term(1, 0)});
    test({2,3}, {term(2, 1)});
    test({3,4}, {term(3, 0), term(4, 0)});
    test({0,1,3}, {term(0, 1), term(1, 2)});
    test({1,2,4}, {term(2, 0), term(1, 0), term(4, 0)});
    test({2,3,6}, {term(2, 4), term(2, 1)});
    test({3,4,8}, {term(3, 0), term(8, 0), term(4, 0)});
    
    test({}, {});
    test({0}, {term(0, 0)});
    test({1}, {term(1, 0)});
    test({2}, {term(2, 0)});
    test({3}, {term(3, 0)});
    test({0,1}, {term(0, 1)});
    test({1,2}, {term(2, 0), term(1, 0)});
    test({2,3}, {term(2, 1)});
    test({3,4}, {term(3, 0), term(4, 0)});
    test({0,1,3}, {term(0, 1), term(1, 2)});
    test({1,2,4}, {term(2, 0), term(1, 0), term(4, 0)});
    test({2,3,6}, {term(2, 4), term(2, 1)});
    test({3,4,8}, {term(3, 0), term(8, 0), term(4, 0)});
    test({0,1,3}, {term(0, 1), term(1, 2)});
    test({1,2,4,9}, {term(2, 0), term(1, 8), term(4, 0)});
    test({2,3,6,13}, {term(2, 1), term(2, 4), term(13, 0)});
    test({3,4,8,12}, {term(3, 0), term(8, 4), term(4, 8)});
}

TEST(computePrimes, DoesntHaveTheBug) {
    test({5,6,7,9,11,13,14,15,16,17,18,19,20,22,23}, {
        term(5,10),term(6,9),term(6,17),term(9,6),term(16,3),term(16,6),term(18,5)
    });
}

auto testC = [](size_t len, const BitVector* cubes, std::vector<BitVector> expected) {
    BitVector* result = compute_primes(len, cubes);
    size_t outputSize = result[0];
    std::vector<BitVector> resultVec;
    for (int i = 0; i < outputSize; i++) {
        resultVec.push_back(result[i]);
    }
    EXPECT_EQ(resultVec, expected);
    free(result);
};

TEST(compute_primes, IsEmptyForTheEmptyList) {
    testC(0, {}, {1});
}

TEST(compute_primes, ContainsTheSizeOnePrimeForOneElement) {
    BitVector cubes[] = {0};
    testC(1, cubes, {3, 0, 0});
}

TEST(compute_primes, ItWorksForMoreElements) {
    BitVector cubes[] = {1,2,4,9};
    testC(4, cubes, {7, 4, 0, 1, 8, 2, 0});
}

