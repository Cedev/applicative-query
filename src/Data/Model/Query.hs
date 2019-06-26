{-# LANGUAGE PolyKinds, GADTs #-}

module Data.Model.Query (
    Query (..)
) where


data Query d a where
    Constant :: a -> Query d a
    Selected :: String -> Query d a
  
  
-- Special arrow
-- (Query d' a -> Query d (Query d' b)) -> Query d a -> Query d b
-- or some constrained category thing

