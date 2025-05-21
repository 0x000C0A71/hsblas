{-|
Module      : BLAS.Internal.Utils
Stability   : unstable
Description : Helper functions

This module contains some internal stuff used to pass the Haskell->C boundary
-}
module BLAS.Internal.Utils where

import Foreign.C.Types (CInt, CChar)

import BLAS.Types

toCInt :: Int -> CInt
toCInt = fromIntegral


layoutToInt :: Layout -> CInt
layoutToInt RowMajor = 101
layoutToInt ColMajor = 102

-- for the cblas fucntions
transposeToInt :: Transpose -> CInt
transposeToInt NoTrans   = 111
transposeToInt Trans     = 112
transposeToInt ConjTrans = 113

-- for the lapacke functions
transposeToInt2 :: Transpose -> CChar
transposeToInt2 NoTrans   = 78
transposeToInt2 Trans     = 84
transposeToInt2 ConjTrans = undefined


