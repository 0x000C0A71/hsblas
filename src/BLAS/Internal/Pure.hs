

{-|
Module      : BLAS.Internal.Pure
Stability   : stable
Description : Immutable wrappers around the immutable BLAS/LAPACK functions

This module provides pleasent wrappers over the functions of BLAS/LAPACK that are already
pure and do not lose functionality or performance when adding immutability.

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
module BLAS.Internal.Pure
    ( dot
    ) where

import qualified BLAS.Direct.Generic as D

import qualified Data.Vector.Storable as V

import BLAS.Internal.Utils

import System.IO.Unsafe

-- | Computes the dot product \(\vec{x}\cdot\vec{y}\) where:
--
-- * \(\vec{x}\in\texttt{a}^{n}\)
-- * \(\vec{y}\in\texttt{a}^{n}\)
dot
    :: D.Blasable a
    => Int -- ^ the stride between successive elements in \(\vec{x}\)
    -> Int -- ^ the stride between successive elements in \(\vec{y}\)
    -> Int -- ^ n
    -> V.Vector a -- ^ The vector containing \(\vec{x}\)'s data
    -> V.Vector a -- ^ The vector containing \(\vec{y}\)'s data
    -> a
dot incX incY n x y = unsafePerformIO $ do
    V.unsafeWith x $ \px ->
        V.unsafeWith y $ \py ->
            D.dot (toCInt n) px (toCInt incX) py (toCInt incY)




