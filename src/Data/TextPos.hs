{-# LANGUAGE StrictData #-}
module Data.TextPos where

data Pos = Pos
  { char :: Word
  , line :: Word
  , col  :: Word
  } deriving (Eq, Ord, Read, Show)

move :: Char -> Pos -> Pos
move x = \ (Pos k m n) -> Pos (k+1) `uncurry` case x of
    '\n' -> (m+1, 0)
    '\r' -> (m,   0)
    '\b' -> (m, case n of 0 -> 0; _ -> n-1)
    _    -> (m, n+1)
