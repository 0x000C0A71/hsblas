
{-|
Module      : BLAS.Internal.Wrapped
Stability   : stable
Description : Immutable wrappers around the mutable BLAS/LAPACK functions

This module provides pleasent stateless wrappers over the stateful functions of
"BLAS.Internal.Mutable", sacrificing some functionality for immutable data

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
module BLAS.Internal.Wrapped
    ( gemm
    ) where

import BLAS.Internal.Utils
import BLAS.Types
import qualified BLAS.Direct.Generic as D
import qualified BLAS.Internal.Mutable as M

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import System.IO.Unsafe


-- | Computes the matrix \(C=\alpha AB\) where:
--
-- * \(A\in\texttt{a}^{n\times k}\)
-- * \(B\in\texttt{a}^{k\times m}\)
-- * \(C\in\texttt{a}^{n\times m}\)
-- * \(\alpha\in\texttt{a}\)
gemm
    :: D.Blasable a
    => Layout     -- ^ Whether the matrices are row-major or column-major
    -> Transpose  -- ^ Whether to transpose the \(A\) matrix before processing
    -> Transpose  -- ^ Whether to transpose the \(B\) matrix before processing
    -> Int        -- ^ The dimension \(\vec{n}\)
    -> Int        -- ^ The dimension \(\vec{m}\)
    -> Int        -- ^ The dimension \(\vec{k}\)
    -> Int        -- ^ The stride between lines in the vector in the \(A\) matrix
    -> Int        -- ^ The stride between lines in the vector in the \(B\) matrix
    -> a          -- ^ The scalar \(\alpha\) by which the result is scaled
    -> V.Vector a -- ^ The components of matrix \(A\)
    -> V.Vector a -- ^ The components of matrix \(B\)
    -> V.Vector a -- ^ The components of matrix \(C\)
gemm l ta tb n m k lda ldb alpha a b = unsafePerformIO $ do
    c <- VM.unsafeNew (m*n)
    M.gemm l ta tb n m k lda ldb ldc alpha D.zero a b c
    V.unsafeFreeze c
    where
        ldc = case l of
            RowMajor -> m
            ColMajor -> n

