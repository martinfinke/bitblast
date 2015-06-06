#ifndef qmc_qmc_priv_h
#define qmc_qmc_priv_h

#include "qmc.h"
#include <unordered_set>
#include <vector>

struct QmTermHash {
    std::size_t operator () ( const QmTerm& p ) const {
        // Modified Bernstein hash
        // http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx
        
        return ( 33 * p.term ) ^ p.mask;
    }
};



inline bool operator <(const QmTerm &a, const QmTerm &b) {
    return a.term < b.term;
}
inline bool operator ==(const QmTerm &a, const QmTerm &b) {
    return a.term == b.term && a.mask == b.mask;
}



std::unordered_set<QmTerm, QmTermHash> computePrimes(std::vector<BitVector> cubes);

#endif
