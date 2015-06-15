module TruthBased where

import Variable

expand :: Int -> TruthTable -> [TruthTable]
expand 0 table = [table]