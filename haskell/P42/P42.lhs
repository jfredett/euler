> module P42 where

===============================================================================
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so 
the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical 
position and adding these values we form a word value. For example, the word value
for SKY is 19 + 11 + 25 = 55 = 10. If the word value is a triangle number then 
we shall call the word a triangle word.

Using words.txt, a 16K text file containing nearly two-thousand common 
English words, how many are triangle words?
===============================================================================

> import Data.Char 
> import PELib.PELib(integralSqrt)

> stringToListOfNumbers :: String -> [Int]
> stringToListOfNumbers s = map (succ . (subtract (ord 'A')) . ord) s

a triangle number has the form

T(n) = n(n+1) / 2, solving for n we get:

0 = n^2 + n - 2T(n) => via the quadratic formula

n = (-1 +/- sqrt(1 + 4*2T(n))) / 2 

we know T(n) > 0, so sqrt(1 + 8T(n)) > 0, and since n > 0, we can choose n = (sqrt(1 + 8T(n)) - 1) / 2

so now we have a predicate, but we need to have an integral square root function, the one that we wrote for 
problem 39 will do nicely, however since it returns a Maybe Integer, we'll need to make an instance of Num for 
Maybe Integer


> instance Num (Maybe Integer) where
>	(Just x) + (Just y) = Just (x + y)
>	_ + _ = Nothing

>	(Just x) * (Just y) = Just (x * y)
>	_ * _ = Nothing

>	(Just x) - (Just y) = Just (x - y)
>	_ - _ = Nothing 

>	abs (Just x) = Just (abs x)
>	abs _ = Nothing

> 	signum (Just x) = Just (signum x)
>	signum _ 	= Nothing

>	fromInteger x	= Just x

> mdiv :: Maybe Integer -> Integer -> Maybe Integer
> mdiv (Just x) y = Just (x `div` y)
> mdiv Nothing _ = Nothing
	
------------------------------------------

> isTriangle :: Integer -> Bool
> isTriangle x = case ((integralSqrt (1 + 8*x) - 1) `mdiv` 2) of 
>	Nothing -> False
>	Just _ -> True

------------------------------------------


now we can read in the "words.txt" 

> readWords = do
>	wordstxt <- readFile "./P42/words.txt"
>	let wordstxt' = (read $ "[" ++ wordstxt ++ "]")::[String]
>	return wordstxt'


Now solvefortytwo is

> solvefortytwo = do 
>	wordstxt <- readWords
>	putStrLn $ (show . length 
>		 .  filter (isTriangle . fromIntegral . sum . stringToListOfNumbers) ) wordstxt


