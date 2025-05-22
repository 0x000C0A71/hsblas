{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import BLAS
import qualified Data.Vector.Storable as V

import Test.Tasty
import Test.Tasty.HUnit



correctnessTests = testGroup "Correctness"
    [ testCase "`dot`" $
        let a = V.fromList [1,2,3] :: V.Vector Double
            b = V.fromList [4,5,6]

            c = dot 1 1 3 a b
            t = V.sum $ V.zipWith (*) a b
        in c @?= t
    , testGroup "`gemm`"
        [ testCase "Identity" $
            let a = V.fromList [1,2,3,4] :: V.Vector Double
                b = V.fromList [1,0,0,1]
                c = gemm RowMajor NoTrans NoTrans 2 2 2  2 2  1 a b
            in c @?= a
        , testCase "Transpose" $
            let a = V.fromList [1,2,3,4] :: V.Vector Double
                b = V.fromList [1,0,0,1]
                c = gemm RowMajor Trans NoTrans 2 2 2  2 2  1 a b
            in c @?= V.fromList [1,3,2,4]
        , testCase "Submatrix" $
            let a = V.fromList [1,2,0,3,4,0] :: V.Vector Double
                b = V.fromList [1,0,0,1]
                c = gemm RowMajor NoTrans NoTrans 2 2 2  3 2  1 a b
            in c @?= V.fromList [1,2,3,4]
        , testCase "Matmul" $
            let a = V.fromList [1,2,3,4] :: V.Vector Double
                b = V.fromList [5,6,7,8]
                c = gemm RowMajor NoTrans NoTrans 2 2 2  2 2  1 a b
            in c @?= V.fromList
                [1*5 + 2*7, 1*6 + 2*8
                ,3*5 + 4*7, 3*6 + 4*8]
        , testCase "ColMajor" $
            let a = V.fromList [1,3,2,4] :: V.Vector Double
                b = V.fromList [5,7,6,8]
                c = gemm ColMajor NoTrans NoTrans 2 2 2  2 2  1 a b
            in c @?= V.fromList
                [1*5 + 2*7, 3*5 + 4*7
                ,1*6 + 2*8, 3*6 + 4*8]
        , testCase "Non-square" $
            let a = V.fromList
                    [ 1, 2, 3
                    , 4, 5, 6] :: V.Vector Double
                b = V.fromList
                    [ 7, 8
                    , 9,10
                    ,11,12]
                c = gemm RowMajor NoTrans NoTrans 2 2 3  3 2  1 a b
            in c @?= V.fromList
                [1*7 + 2*9 + 3*11, 1*8 + 2*10 + 3*12
                ,4*7 + 5*9 + 6*11, 4*8 + 5*10 + 6*12]
        ]
    , testGroup "`gesv`"
        [ testCase "Inverse" $
            let a = V.fromList [1,2,3,4] :: V.Vector Double
                b = V.fromList [1,0,0,1]
            in case gesv RowMajor 2 2 2 2 a b of
                    Just x  ->
                        let b' = gemm RowMajor NoTrans NoTrans 2 2 2  2 2  1 a x
                            abserr = V.sum $ V.map abs $ V.zipWith (-) b' b
                            errmsg = "Absolute error should be at least below 1e-12, but is " ++ show abserr
                        in abserr < 1e-12 @? errmsg
                    Nothing -> assertFailure "Got solve faliure, but solve should be possible!"
        ]
    ]



main :: IO ()
main = defaultMain $ testGroup "hsblas Tests" [correctnessTests]



