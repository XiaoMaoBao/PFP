module Main where

import Lib

main :: IO ()
main = someFunc



-- Probability distribution
newtype Prob a = Prob {runProb :: [(a, Rational)]}

-- die object
die' :: Prob Integer
die' = Prob [(x, 1/6) | x <- [1..6]]

