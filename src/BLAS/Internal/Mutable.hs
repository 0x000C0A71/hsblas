
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
    , gesv
    ) where

import BLAS.Internal.Utils
import BLAS.Types
import qualified BLAS.Direct.Generic as D

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- TODO: Move from IO to some primitive state à la vectors


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
    -> Int                            -- ^ The dimension \(n\)
    -> Int                            -- ^ The dimension \(m\)
    -> Int                            -- ^ The dimension \(k\)
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


-- | Solves \(AX=B\) for \(X\) where:
--
-- * \(A\in\texttt{a}^{n\times n}\)
-- * \(X\in\texttt{a}^{n\times nrhs}\)
-- * \(B\in\texttt{a}^{n\times nrhs}\)
--
-- The solve is performed via LU-Decomposition, where the matrix is decomposed into:
--
-- * \(P\): A permutation matrix
-- * \(L\): A lower triangualar matrix
-- * \(U\): An upper triangular matrix
--
-- such that \(PLU = A\)
--
-- \(A\) is replaced inplace with the \(L\) & \(U\) matrices, and the \(P\) matrix's pivot
-- indices are returned as part of the return value. They are encoded as a
-- 'Data.Vector.Storable.Vector' of `Int`s, that specifies for each row, with which row it
-- was interchanged.
--
-- As a solve is not always possible, this operation can go either `Right`, or `Left`.
-- A `Left` indicates that a solve is not possible. Even still, the factorisation into
-- \(P\), \(L\) & \(U\) is still performed and returned/replaced inplace.
gesv
    :: D.Blasable a
    => Layout -- ^ Whether the matrices are row-major or column-major
    -> Int    -- ^ The dimension \(n\)
    -> Int    -- ^ The dimension \(nrhs\)
    -> Int    -- ^ The stride between lines in the vector in the \(A\) matrix
    -> Int    -- ^ The stride between lines in the vector in the \(B\) matrix

    -- | The components of matrix \(A\)
    --
    -- Gets overwritten with \(L\) & \(U\) where the unit diagonal of \(L\)
    -- is not stored
    -> VM.MVector (VM.PrimState IO) a

    -- | The components of matrix \(B\)
    --
    -- Gets overwritten with the solved \(X\)
    -> VM.MVector (VM.PrimState IO) a

    -- | If the operation goes `Right`, returns the pivot indices,
    -- if it goes `Left` (as a solve is impossible), returns the
    -- pivot indices and the index along the diagonal of \(U\) at which
    -- factorisation yielded a 0. In this case, \(P\), \(L\) & \(U\)
    -- are still computed and returned in their respective manner.
    -> IO (Either
        (V.Vector Int, Int)
        (V.Vector Int))
gesv l n nrhs lda ldb a b = do
    cipiv <- VM.unsafeNew n
    info <- VM.unsafeWith a $ \pa ->
        VM.unsafeWith b $ \pb ->
            VM.unsafeWith cipiv $ \pipiv ->
                D.gesv
                    (layoutToInt l)
                    (toCInt n)
                    (toCInt nrhs)
                    pa
                    (toCInt lda)
                    pipiv
                    pb
                    (toCInt ldb)
    ipiv <- V.map fromCInt <$> V.unsafeFreeze cipiv
    return $ case info `compare` 0 of
        EQ -> Right ipiv
        GT -> Left (ipiv, fromCInt info)
        LT -> error "TODO: hsblas handle illegal arguments (`gesv`)"
