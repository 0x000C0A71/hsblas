{-|
Module      : BLAS.Direct.Generic
Stability   : unstable
Description : Generification of the raw imported C functions

This module generalizes over the datatypes that BLAS/LAPACK supports.
Instead of calling `D.cblas_dgemm` or `D.cblas_sgemm`, this module allows you to just call
`gemm`, and the specific implementation is infered from the type of the values
passed.

Using the methods from this module is considered an advanced feature, and is little more
than the direct C imports.
-}
module BLAS.Direct.Generic
    ( Blasable(..)
    ) where



import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import qualified BLAS.Direct as D

-- | This class generalizes the CBLAS/LAPACK functions
--
-- A type @a@ having a 'Blasable' instance implies, that vectors/matrices of type @a@ can be accelerated.
-- While it can be relatively common to see @Blasable a@ as a constraint (even in user code),
-- calls/implementations of its methods are intended for internal use (their documentation might also be
-- hidden wherever you read this. See "BLAS.Direct.Generic" for full documentation).
--
-- Even though, instancing/using the 'Blasable' class directly is mostly intended for internal use, it is
-- still exposed through the "BLAS.Direct.Generic" module for those wanting low-level access to directly
-- imported CBLAS/LAPACKE functions, in which case it offers a clear advantage over using the functions
-- from "BLAS.Direct" directly, in that it is generic over the types, while not adding any overhead.
--
-- While it is possible to instance this class (in which case an implementation for every function
-- needs to be provided -> the minimal implementation fills every function), it is not really the
-- intention, as instances for all the supported types should already be provided by this module.
-- 
-- Feel free, however to instance it in the off-chance where an instance /does/ make sense
-- (e.g. if you want to support a vendor specific extension to CBLAS/LAPACK, or if you want to
-- implement variants for custom datatypes in pure haskell, to take advantage of functions that
-- use these functions even in the absence of CBLAS/LAPACK acceleration). Keep in mind, however,
-- that these are /comparatively low level/ functions, that, for example, pass multiple items
-- through pointers, that can point to more than one value.
--
-- The documentation for the functions of the 'Blasable' class assume the reader is familiar
-- with CBLAS/LAPACKE, and its function signatures, as the parameters are exactly the ones of
-- the underlying CBLAS/LAPACKE functions in the same order.
class (Storable a, Fractional a) => Blasable a where
    zero :: a -- ^ The additive identity of @a@
    one  :: a -- ^ The multiplactive identity of @a@

    -- | Computes the matrix \(C=\alpha AB+\beta C\) where:
    --
    -- * \(A\in\texttt{a}^{n\times k}\)
    -- * \(B\in\texttt{a}^{k\times m}\)
    -- * \(C\in\texttt{a}^{n\times m}\)
    -- * \(\alpha\in\texttt{a}\)
    -- * \(\beta\in\texttt{a}\)
    gemm
        :: CInt  -- ^ \(\verb|CBLAS_LAYOUT    layout|\)
        -> CInt  -- ^ \(\verb|CBLAS_TRANSPOSE TransA|\)
        -> CInt  -- ^ \(\verb|CBLAS_TRANSPOSE TransB|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT M|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT N|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT K|\)
        -> a     -- ^ \(\verb|const a         alpha|\)
        -> Ptr a -- ^ \(\verb|const a *       A|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT lda|\)
        -> Ptr a -- ^ \(\verb|const a *       B|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT ldb|\)
        -> a     -- ^ \(\verb|const a         beta|\)
        -> Ptr a -- ^ \(\verb|a *             C|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT ldc|\)
        -> IO ()


    -- | Computes the vector \(\vec{y}=\alpha A\vec{x}+\beta\vec{y}\) where:
    --
    -- * \(A\in\texttt{a}^{n\times m}\)
    -- * \(\vec{x}\in\texttt{a}^{m}\)
    -- * \(\vec{y}\in\texttt{a}^{n}\)
    -- * \(\alpha\in\texttt{a}\)
    -- * \(\beta\in\texttt{a}\)
    gemv
        :: CInt  -- ^ \(\verb|CBLAS_LAYOUT    layout|\)
        -> CInt  -- ^ \(\verb|CBLAS_TRANSPOSE TransA|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT M|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT N|\)
        -> a     -- ^ \(\verb|const a         alpha|\)
        -> Ptr a -- ^ \(\verb|const a *       A|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT lda|\)
        -> Ptr a -- ^ \(\verb|const a *       X|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incX|\)
        -> a     -- ^ \(\verb|const a         beta|\)
        -> Ptr a -- ^ \(\verb|a *             Y|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incY|\)
        -> IO ()

    -- | Computes the vector \(\vec{y}=\alpha\vec{x}+\vec{y}\) where:
    --
    -- * \(\vec{x}\in\texttt{a}^{n}\)
    -- * \(\vec{y}\in\texttt{a}^{n}\)
    -- * \(\alpha\in\texttt{a}\)
    axpy
        :: CInt  -- ^ \(\verb|const CBLAS_INT N|\)
        -> a     -- ^ \(\verb|const a         alpha|\)
        -> Ptr a -- ^ \(\verb|const a *       X|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incX|\)
        -> Ptr a -- ^ \(\verb|a *             Y|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incY|\)
        -> IO ()

    -- | Copies \(\vec{x}\) to \(\vec{y}\) where:
    --
    -- * \(\vec{x}\in\texttt{a}^{n}\)
    -- * \(\vec{y}\in\texttt{a}^{n}\)
    copy
        :: CInt  -- ^ \(\verb|const CBLAS_INT N|\)
        -> Ptr a -- ^ \(\verb|const a *       X|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incX|\)
        -> Ptr a -- ^ \(\verb|a *             Y|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incY|\)
        -> IO ()

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
    gesv
        :: CInt     -- ^ \(\verb|int          matrix_layout|\)
        -> CInt     -- ^ \(\verb|lapack_int   n|\)
        -> CInt     -- ^ \(\verb|lapack_int   nrhs|\)

        -- | \(\verb|a *          a|\)
        --
        -- Gets overwritten with L & U where the unit diagonal of L is not stored
        -> Ptr a
        -> CInt     -- ^ \(\verb|lapack_int   lda|\)
        
        -- | \(\verb|lapack_int * ipiv|\)
        --
        -- Outputs the pivot indices of the P matrix through an @int[n]@ array
        -> Ptr CInt

        -- | \(\verb|a *          b|\)
        --
        -- Gets overwritten with the solved X
        -> Ptr a
        -> CInt     -- ^ \(\verb|lapack_int   ldb|\)
        
        -- | \(\verb|int          info|\)
        --
        -- * = 0: Success
        -- * < 0: the (-info)-th argument was illegal
        -- * > 0: Factorisation yielded a 0 at U[info, info] ->
        -- No solve for X could be computed (PLU are still returned)
        -> IO CInt

    -- | Solves \(AX=B\) for \(X\) where the system can be overdetermined or underdetermined, where:
    --
    -- * \(A\in\texttt{a}^{m\times n}\)
    -- * \(X\in\texttt{a}^{n\times nrhs}\)
    -- * \(B\in\texttt{a}^{m\times nrhs}\)
    --
    -- The solve is performed via QR-Decomposition
    -- IO of the arguments is similar to 'gesv'
    gels
        :: CInt  -- ^ \(\verb|int        matrix_layout|\)
        -> CChar -- ^ \(\verb|char       trans|\)
        -> CInt  -- ^ \(\verb|lapack_int m|\)
        -> CInt  -- ^ \(\verb|lapack_int n|\)
        -> CInt  -- ^ \(\verb|lapack_int nrhs|\)
        -> Ptr a -- ^ \(\verb|a *        a|\)
        -> CInt  -- ^ \(\verb|lapack_int lda|\)
        -> Ptr a -- ^ \(\verb|a *        b|\)
        -> CInt  -- ^ \(\verb|lapack_int ldb|\)
        -> IO CInt

    -- | Computes the dot product \(\vec{x}\cdot\vec{y}\) where:
    --
    -- * \(\vec{x}\in\texttt{a}^{n}\)
    -- * \(\vec{y}\in\texttt{a}^{n}\)
    dot
        :: CInt  -- ^ \(\verb|const CBLAS_INT N|\)
        -> Ptr a -- ^ \(\verb|const a *       X|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incX|\)
        -> Ptr a -- ^ \(\verb|const a *       Y|\)
        -> CInt  -- ^ \(\verb|const CBLAS_INT incY|\)
        -> IO a


instance Blasable Double where
    zero = 0
    one  = 1
    gemm = D.cblas_dgemm
    gemv = D.cblas_dgemv
    axpy = D.cblas_daxpy
    copy = D.cblas_dcopy
    gesv = D.lapack_dgesv
    gels = D.lapack_dgels
    dot  = D.cblas_ddot

