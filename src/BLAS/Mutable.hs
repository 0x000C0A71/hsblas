
{-|
Module      : BLAS.Mutable
Stability   : stable
Description : Mutable version of the API

This module exposes the Mutable API of the library

This allows access to all the statefulness of BLAS/LAPACK, allowing users to use all the
optimization strategies they're used to from BLAS/LAPACK. See "BLAS" for the immutable
version of the API
-}

module BLAS.Mutable
    -- * Immutable functions
    ( dot

    -- * Mutable functions
    , gemm

    -- * Types
    , Layout(..)
    , Transpose(..)
    , Blasable
    ) where

import BLAS.Internal.Pure
import BLAS.Internal.Mutable
import BLAS.Types
