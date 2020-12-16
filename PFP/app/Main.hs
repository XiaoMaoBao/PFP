module Main where

import Lib

main :: IO ()
main = someFunc



-- Probability distribution
newtype Prob a = Prob {runProb :: [(a, Rational)]}

-- die object
die :: Prob Integer
die = Prob [(x, 1/6) | x <- [1..6]]


-- the object die represent the probability distribution of a die
-- with a 1/6 chance of landing either a 1,2,3,4,5 or a 6

--monadic 
--functor fmap f fa 
-- where fmap :: (a - > b) -> fa -> fb
-- f maps a -> b
-- fa is a data structure f that wraps an object a
-- fb is a data structure f which wraps an object b, that was constructe by f :: a -> b
instance Functor Prob where
    fmap f xs = Prob [(f x, p) | (x, p) <- runProb xs]

instance Applicative Prob where
    pure x = Prob [(x,1)]
    fs <*> xs
        = Prob
        [ (f x,fp*xp) | (f,fp) <- runProb fs, (x,xp) <- runProb xs ]

instance Monad Prob where
    xs >>= f  = Prob
        [ (y,xp*yp)
        | (x,xp) <- runProb xs
        , (y,yp) <- runProb (f x) ]


support :: Prob a -> [a]
support = fmap fst .  runProb

expect :: (a -> Rational) -> Prob a -> Rational
expect p xs = sum [ p x * xp | (x,xp) <- runProb xs ]

--probOf predicate (a - > Bool)
--       a prob object with a type a
--

probOf :: (a -> Bool) -> Prob a -> Rational
probOf p = expect (bool . p)
    where bool x | x  = 1
                 | otherwise  = 0


uniform xs = Prob [ (x,n) | x <- xs ]
  where
    n = 1 / toEnum (length xs)
    
die' :: Prob Integer
die' = uniform [1..6]

probOfTwoDicesWhen :: Integer -> Rational
probOfTwoDicesWhen b = do probOf (b==) $ do x <- die'
                                            y <- die'
                                            pure (x + y)

