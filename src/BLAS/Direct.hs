{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : BLAS.Direct
Stability   : unstable
Description : Direct C imports

This module contains the direct FFI imports of the CBLAS/LAPACKE functions

It is intended to be used only internally, as more pleasent wrappers are
exposed aswell (see "BLAS.Direct.Generic"). As such, there is no guarantee on API stability for these
functions. It is, however, still exposed to serve those wanting to use these
directly, providing low level access. If you want to use CBLAS/LAPACK functions
directly, but do not want to deal with linking the C libraries in your code,
this is for you.

Each function is annotated with the original C function's argument name and type
which should provide all the necessary info to use this while referencing the
documentation of your favorite CBLAS/LAPACKE implementation.
-}
module BLAS.Direct
    ( cblas_dgemm
    , cblas_dgemv
    , cblas_daxpy
    , cblas_dcopy
    , lapack_dgesv
    , lapack_dgels
    , cblas_ddot
    ) where

import Foreign.C.Types
import Foreign.Ptr




foreign import ccall "cblas_dgemm" cblas_dgemm
    :: CInt       -- ^ \(\verb|CBLAS_LAYOUT    layout|\)
    -> CInt       -- ^ \(\verb|CBLAS_TRANSPOSE TransA|\)
    -> CInt       -- ^ \(\verb|CBLAS_TRANSPOSE TransB|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT M|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT N|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT K|\)
    -> Double     -- ^ \(\verb|const double    alpha|\)
    -> Ptr Double -- ^ \(\verb|const double *  A|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT lda|\)
    -> Ptr Double -- ^ \(\verb|const double *  B|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT ldb|\)
    -> Double     -- ^ \(\verb|const double    beta|\)
    -> Ptr Double -- ^ \(\verb|double *        C|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT ldc|\)
    -> IO ()


foreign import ccall "cblas_dgemv" cblas_dgemv
    :: CInt       -- ^ \(\verb|CBLAS_LAYOUT    layout|\)
    -> CInt       -- ^ \(\verb|CBLAS_TRANSPOSE TransA|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT M|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT N|\)
    -> Double     -- ^ \(\verb|const double    alpha|\)
    -> Ptr Double -- ^ \(\verb|const double *  A|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT lda|\)
    -> Ptr Double -- ^ \(\verb|const double *  X|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incX|\)
    -> Double     -- ^ \(\verb|const double    beta|\)
    -> Ptr Double -- ^ \(\verb|double *        Y|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incY|\)
    -> IO ()



foreign import ccall "cblas_daxpy" cblas_daxpy
    :: CInt       -- ^ \(\verb|const CBLAS_INT N|\)
    -> Double     -- ^ \(\verb|const double    alpha|\)
    -> Ptr Double -- ^ \(\verb|const double *  X|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incX|\)
    -> Ptr Double -- ^ \(\verb|double *        Y|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incY|\)
    -> IO ()

foreign import ccall "cblas_dcopy" cblas_dcopy
    :: CInt       -- ^ \(\verb|const CBLAS_INT N|\)
    -> Ptr Double -- ^ \(\verb|const double *  X|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incX|\)
    -> Ptr Double -- ^ \(\verb|double *        Y|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incY|\)
    -> IO ()



foreign import ccall "LAPACKE_dgesv" lapack_dgesv
    :: CInt       -- ^ \(\verb|int          matrix_layout|\)
    -> CInt       -- ^ \(\verb|lapack_int   n|\)
    -> CInt       -- ^ \(\verb|lapack_int   nrhs|\)
    -> Ptr Double -- ^ \(\verb|double *     a|\)
    -> CInt       -- ^ \(\verb|lapack_int   lda|\)
    -> Ptr CInt   -- ^ \(\verb|lapack_int * ipiv|\)
    -> Ptr Double -- ^ \(\verb|double *     b|\)
    -> CInt       -- ^ \(\verb|lapack_int   ldb|\)
    -> IO CInt

foreign import ccall "LAPACKE_dgels" lapack_dgels
    :: CInt       -- ^ \(\verb|int        matrix_layout|\)
    -> CChar      -- ^ \(\verb|char       trans|\)
    -> CInt       -- ^ \(\verb|lapack_int m|\)
    -> CInt       -- ^ \(\verb|lapack_int n|\)
    -> CInt       -- ^ \(\verb|lapack_int nrhs|\)
    -> Ptr Double -- ^ \(\verb|double *   a|\)
    -> CInt       -- ^ \(\verb|lapack_int lda|\)
    -> Ptr Double -- ^ \(\verb|double *   b|\)
    -> CInt       -- ^ \(\verb|lapack_int ldb|\)
    -> IO CInt

foreign import ccall "cblas_ddot" cblas_ddot
    :: CInt       -- ^ \(\verb|const CBLAS_INT N|\)
    -> Ptr Double -- ^ \(\verb|const double *  X|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incX|\)
    -> Ptr Double -- ^ \(\verb|const double *  Y|\)
    -> CInt       -- ^ \(\verb|const CBLAS_INT incY|\)
    -> IO Double

