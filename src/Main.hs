module Main where

import Arithmetics
import CoinCBCInterface
import Formula
import MinimizeFormula
import NormalForm
import QmcCpp
import QmcTypes
import Tseitin
import TseitinSelect
import Variable

import System.Environment(getArgs)
import qualified Data.Set as Set
import Data.List(sortBy)
import Data.Ord(comparing)

import Test.Hspec
import Test.QuickCheck
import VariableSpec

vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = makeVars 10
varSet = Set.fromList vars
[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars
-- -(0 XOR 1) && ((0 XOR 1) <=> 2)
testF = And [Not $ Xor [x0,x1], Equiv [Xor [x0,x1], x2]]
