module Utils where

import qualified Data.Set as Set

firstOfTriple :: (a,b,c) -> a
firstOfTriple (x,_,_) = x

thirdOfTriple :: (a,b,c) -> c
thirdOfTriple (_,_,z) = z