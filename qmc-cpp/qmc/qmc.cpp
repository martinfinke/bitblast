/*
 *  qmc.cp
 *  qmc
 *
 *  Created by Martin on 06.06.15.
 *  Copyright (c) 2015 Martin Finke. All rights reserved.
 *
 */


#include "qmc_priv.h"

#include <set>
#include <unordered_set>
#include <vector>
#include <bitset>
#include <utility> // std::pair
#include <algorithm> // std::set_union
#import <cstdlib>


#pragma GCC visibility push(hidden)

int answer_to_everything(int i) {return i*42;}

size_t bitcount(BitVector i) {
    size_t res = 0;
    while (i > 0) {
        res += i&1;
        i>>=1;
    }
    return res;
}

inline bool isPowerOfTwoOrZero(BitVector x) {
    return (x & (~x + 1)) == x;
}

inline std::pair<bool,QmTerm> merge(const QmTerm& i, const QmTerm& j) {
    if (i.mask != j.mask) {
        return {false, QmTerm()};
    }
    BitVector y = i.term ^ j.term;
    if (!isPowerOfTwoOrZero(y)) {
        return {false, QmTerm()};
    }
    
    return {true, {
        .term = i.term & j.term,
        .mask = i.mask | y
    }};
}

// https://github.com/prekageo/optistate/blob/master/qm.py
std::unordered_set<QmTerm, QmTermHash> computePrimes(std::vector<BitVector> cubes) {
    std::vector<std::unordered_set<QmTerm, QmTermHash> > sigma;
    for (int i : cubes) {
        auto bc = bitcount(i);
        if (bc >= sigma.size()) {
            sigma.resize(bc+1);
        }
        sigma[bc].insert({.term=i,.mask=0});
    }
    std::unordered_set<QmTerm, QmTermHash> primes;
    while (!sigma.empty()) {
        std::vector<std::unordered_set<QmTerm, QmTermHash> > nsigma;
        std::unordered_set<QmTerm, QmTermHash> redundant;
        for (int i = 0; i < sigma.size() - 1; i++) {
            auto c1 = sigma[i];
            auto c2 = sigma[i+1];
            std::unordered_set<QmTerm, QmTermHash> nc;
            for (auto a : c1) {
                for (auto b : c2) {
                    auto result = merge(a,b);
                    if (result.first) {
                        nc.insert(result.second);
                        redundant.insert(a);
                        redundant.insert(b);
                    }
                }
            }
            nsigma.push_back(nc);
        }
        
        for (auto _cubes : sigma) {
            for (auto c : _cubes) {
                if (redundant.find(c) == redundant.end()) {
                    primes.insert(c);
                }
            }
        }
        sigma = nsigma;
    }
    return primes;
}

BitVector* compute_primes(size_t len, const BitVector* cubes) {
    std::vector<BitVector> cubesVector;
    cubesVector.assign(cubes, cubes+len);
    
    auto primes = computePrimes(cubesVector);
    
    std::vector<BitVector> outputArray;
    for (auto prime : primes) {
        outputArray.push_back(prime.term);
        outputArray.push_back(prime.mask);
    }
    size_t outputLength = outputArray.size() + 1;
    outputArray.insert(outputArray.begin(), outputLength);

    BitVector* out = (BitVector*) malloc(outputArray.size() * sizeof(BitVector));
    for (int i = 0; i < outputArray.size(); i++) {
        out[i] = outputArray[i];
    }
    return out;
}

#pragma GCC visibility pop
