

{-|
Module      : BLAS.Types
Stability   : stable
Description : Contains common types

This module contains types used all over.

It also reexports the 'Blasable' typeclass without exposing its methods.
If this typeclass is needed as a constraint, "BLAS.Direct.Generic" does not have
to be imported, and importing this module is enough.
-}
module BLAS.Types
    ( Layout(..)
    , Transpose(..)
    , Blasable
    ) where

import BLAS.Direct.Generic

-- | Specifies the layout of the matrix in memory
data Layout
    = RowMajor
    | ColMajor
    deriving (Show, Eq)

-- | Specifies how a matrix should be interpreted by BLAS/LAPACK
-- 
-- This allows transposes to be /sneaked into/ computations for free. For example, if I have
-- 2 matrices \(A\) & \(B\) and I want to compute \(A^TB\), I do not have to compute the transpose, and pass the
-- result to 'BLAS.gemm', I can just call 'BLAS.gemm' directly and mark it with 'Trans' to signify
-- the matrix need to be transposed first. Note, that this does not actually
-- /perform a transpose operation/, but instead just read the matrix from memory in a different fashion.
data Transpose
    = NoTrans   -- ^ To be interpreted as is
    | Trans     -- ^ To interpret with a transpose applied
    | ConjTrans -- ^ To interpret with transpose and conjugation
    deriving (Show, Eq)

