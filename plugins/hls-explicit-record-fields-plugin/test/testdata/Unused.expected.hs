{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Unused where

data MyRec = MyRec
  { foo :: Int
  , bar :: Int
  , foobar :: Char
  }

convertMe :: MyRec -> String
convertMe MyRec {foo, bar} = show foo ++ show bar
