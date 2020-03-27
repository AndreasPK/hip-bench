{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Control.Monad
import           Criterion.Main
-- import           Data.Array.Repa                     as R
-- import           Data.Array.Repa.Eval                as R
-- import           Data.Array.Repa.Repr.Unboxed        as R
-- import           Data.Array.Repa.Stencil             as R
-- import           Data.Array.Repa.Stencil.Dim2        as R

import           Graphics.Image                      as I
import           Graphics.Image.Interface            as I
import           Graphics.Image.Interface.Repa
import           Prelude                             as P
import Graphics.Image.IO
import Data.Primitive.Array
import Load

-- {-# NOINLINE load #-}
-- load what = do
--     !thing <- readImage what :: IO (Either String (Image RSU Y Double))
--     Right !img <- pure thing
--     writeImage "tmp.png" (img)
--     return img
    -- return thing

main :: IO ()
main = do
  defaultMain
    [ bgroup
        -- "Gaussian"
        -- [ bench "Native VU" $ whnf (applyFilter gb) imgU'
        -- ]
        "Sobel"
        [
        --   bench "naive U" $ whnf compute sobelU
          bench "loadFrog" $ whnfIO (load "frog.jpeg" )
        -- , bench "Filter R" $ whnf compute sobelFSep
        -- , bench "Filter VU" $ whnf sobelFSep' imgU'
        -- , bench "repa R Agorithms Separated" $ whnfIO sobelRAlgSep
        ]
    ]