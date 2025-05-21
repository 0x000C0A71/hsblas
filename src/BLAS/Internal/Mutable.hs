
{-|
Module      : BLAS.Internal.Mutable
Stability   : stable
Description : Mutable wrappers around the mutable BLAS/LAPACK functions

This module provides pleasent stateful wrappers over the functions of BLAS/LAPACK that would
lose functionality or performance when wrapped into stateless functions, allowing its user
to get every little bit of functionality out of BLAS/LAPACK

= Note
This is intedended for internal use only. There are 3 internal modules ("BLAS.Internal.Pure",
"BLAS.Internal.Mutable", "BLAS.Internal.Wrapped") which are reexported through non-internal
modules:

["BLAS"]: Reexports "BLAS.Internal.Pure" & "BLAS.Internal.Wrapped" giving its user a stateless
interface to use, sacrificing some functionality that would require cloning vectors all over

["BLAS.Mutable"] Reexports "BLAS.Internal.Pure" & "BLAS.Internal.Mutable" providing the user
with all the statefullnes of the BLAS/LAPACK libraries, giving its user the full functionality
yielding optimization opportunities that the stateless API cannot.

The intent is to allow each user to decide for themselves wether to import "BLAS" or "BLAS.Mutable"
-}
module BLAS.Internal.Mutable
    ( gemm
    ) where

import BLAS.Internal.Utils
import BLAS.Types
import qualified BLAS.Direct.Generic as D

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- | Computes the matrix \(C=\alpha AB+\beta C\) where:
--
-- * \(A\in\texttt{a}^{n\times k}\)
-- * \(B\in\texttt{a}^{k\times m}\)
-- * \(C\in\texttt{a}^{n\times m}\)
-- * \(\alpha\in\texttt{a}\)
-- * \(\beta\in\texttt{a}\)
gemm
    :: D.Blasable a
    => Layout                         -- ^ Whether the matrices are row-major or column-major
    -> Transpose                      -- ^ Whether to transpose the \(A\) matrix before processing
    -> Transpose                      -- ^ Whether to transpose the \(B\) matrix before processing
    -> Int                            -- ^ The dimension \(\vec{n}\)
    -> Int                            -- ^ The dimension \(\vec{m}\)
    -> Int                            -- ^ The dimension \(\vec{k}\)
    -> Int                            -- ^ The stride between lines in the vector in the \(A\) matrix
    -> Int                            -- ^ The stride between lines in the vector in the \(B\) matrix
    -> Int                            -- ^ The stride between lines in the vector in the \(C\) matrix
    -> a                              -- ^ The scalar \(\alpha\) by which the result is scaled
    -> a                              -- ^ The scalar \(\beta\)  by which the result is scaled
    -> V.Vector a                     -- ^ The components of matrix \(A\)
    -> V.Vector a                     -- ^ The components of matrix \(B\)
    -> VM.MVector (VM.PrimState IO) a -- ^ The components of matrix \(C\)
    -> IO ()
gemm l ta tb n m k lda ldb ldc alpha beta a b c = do
    V.unsafeWith a $ \pa ->
        V.unsafeWith b $ \pb ->
            VM.unsafeWith c $ \pc ->
                D.gemm
                    (layoutToInt l)
                    (transposeToInt ta)
                    (transposeToInt tb)
                    n' m' k'
                    alpha
                    pa
                    (toCInt lda)
                    pb
                    (toCInt ldb)
                    beta
                    pc
                    (toCInt ldc)
    where
        n' = toCInt n
        m' = toCInt m
        k' = toCInt k
