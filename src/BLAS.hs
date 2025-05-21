
{-|
Module      : BLAS
Stability   : stable
Description : Immutable version of the API

This module exposes the immutable API of the library

This API sacrifices some functionality to provide a stateless immutable version of
the BLAS/LAPACK functions. See "BLAS.Mutable" for the mutable version of the API
-}
module BLAS
    -- * Immutable functions
    ( dot

    -- * Immutable wrappers around mutable functions 
    , gemm

    -- * Types
    , Layout(..)
    , Transpose(..)
    , Blasable
    ) where

import BLAS.Internal.Pure
import BLAS.Internal.Wrapped
import BLAS.Types



