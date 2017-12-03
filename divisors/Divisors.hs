module Divisors where

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = case filter (dividable a) [2..a-1] of
  [] -> Left (show a ++ " is prime")
  r -> Right r

dividable :: (Integral a) => a -> a -> Bool
dividable what by = (what `mod` by) == 0
