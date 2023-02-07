{-# OPTIONS_GHC -O #-}

module Sigs where

f1 :: Int -> Int -> Int
f1 x 1 = x + 5
f1 x _ = x + 10
