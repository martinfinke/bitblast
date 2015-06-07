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
#include <algorithm> // std::for_each
#include <cstdlib>
#include <iostream>
#include <limits>
#include <thread>
#include <functional>

#include "concurrentqueue.h"

using moodycamel::ConcurrentQueue;


#pragma GCC visibility push(hidden)

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

void dequeueRedundant(ConcurrentQueue<QmTerm>& redundantQueue, std::unordered_set<QmTerm, QmTermHash>& redundant) {
    bool empty = false;
    do {
        QmTerm current;
        empty = !redundantQueue.try_dequeue(current);
        if (!empty) {
            redundant.insert(std::move(current));
        }
    } while (!empty);
}

void dequeueSigma(ConcurrentQueue<std::pair<size_t, std::unordered_set<QmTerm, QmTermHash> > >& nsigma, std::vector<std::unordered_set<QmTerm, QmTermHash> >& sigma) {
    bool empty = false;
    do {
        std::pair<size_t, std::unordered_set<QmTerm, QmTermHash> > current;
        empty = !nsigma.try_dequeue(current);
        if (!empty) {
            if (current.first >= sigma.size()) {
                sigma.resize(current.first+1);
            }
            sigma[current.first] = std::move(current.second);
        }
    } while (!empty);
}

template<typename FunctionType>
void parallel_for(int numThreads, size_t begin, size_t end, FunctionType func)
{
    size_t delta = (end - begin) / numThreads;
    std::vector<std::thread> threads;
    for (size_t i = 0; i < numThreads; ++i) {
        threads.push_back(std::thread([=]{
            size_t localbegin = begin + i*delta;
            size_t localend = (i == numThreads - 1) ? end : localbegin + delta;
            for (size_t it = localbegin; it < localend; ++it) {
                func(it);
            }
        }));
    }
    for (size_t i = 0; i < numThreads; ++i) {
        threads[i].join();
    }
}

bool compareBitcount(const QmTerm& a, const QmTerm& b) {
    return bitcount(a.term) < bitcount(b.term);
}

// https://github.com/prekageo/optistate/blob/master/qm.py
std::unordered_set<QmTerm, QmTermHash> computePrimes(std::vector<BitVector> cubes) {
    const int numThreads = 10;
    
    
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
        ConcurrentQueue<std::pair<size_t, std::unordered_set<QmTerm, QmTermHash> > > nsigma;
        ConcurrentQueue<QmTerm> redundantQueue;
        
        auto lambda = [&sigma, &nsigma, &redundantQueue](size_t i) {
            auto c1 = sigma[i];
            auto c2 = sigma[i+1];
            std::unordered_set<QmTerm, QmTermHash> nc;
            for (auto a : c1) {
                for (auto b : c2) {
                    auto result = merge(a,b);
                    if (result.first) {
                        nc.insert(result.second);
                        redundantQueue.enqueue(a);
                        redundantQueue.enqueue(b);
                    }
                }
            }
            nsigma.enqueue({i, std::move(nc)});
        };
        
        parallel_for<std::function<void(size_t)> >(numThreads, 0, sigma.size() - 1, lambda);
        
        std::unordered_set<QmTerm, QmTermHash> redundant;
        dequeueRedundant(redundantQueue, redundant);
        
        for (auto _cubes : sigma) {
            for (auto c : _cubes) {
                if (redundant.find(c) == redundant.end()) {
                    primes.insert(c);
                }
            }
        }
        
        sigma.clear();
        dequeueSigma(nsigma, sigma);
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
