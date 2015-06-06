#ifndef qmc_qmc_h
#define qmc_qmc_h

#include <cstddef>


#pragma GCC visibility push(default)


typedef int BitVector;
    
typedef struct {
    BitVector term;
    BitVector mask;
} QmTerm;
    
    
extern "C" int answer_to_everything(int i);

// The first element in the output is the length of the output
extern "C" BitVector* compute_primes(size_t len, const BitVector* cubes);
    
#pragma GCC visibility pop

#endif