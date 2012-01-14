> module P31 where

===============================================================================
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1*£1 + *50p + 2*20p + 1*5p + 1*2p + 3*1p

How many different ways can £2 be made usin any number of coins?
===============================================================================

> import Data.List (partition, union, nub, (\\))
> import Control.Arrow
> import PELib.PELib ((<#>), removeDupes)


> data Combination = C !( Integer, Integer, Integer, Integer
>			, Integer, Integer, Integer, Integer)
>			deriving (Eq,Show)

> combMap :: (Integer -> Integer) -> Combination -> Combination
> combMap z (C (a,b,c,d,e,f,g,h)) = C (z a, z b, z c, z d, z e, z f, z g, z h) 

> combzipWith f	(C (a1,a2,a3,a4,a5,a6,a7,a8)) (C (b1,b2,b3,b4,b5,b6,b7,b8)) = 
>	(C ( f a1 b1, f a2 b2, f a3 b3, f a4 b4, f a5 b5, f a6 b6, f a7 b7, f a8 b8))

> combSum :: Combination -> Integer
> combSum (C (a,b,c,d,e,f,g,h)) = (1*a)   + (2*b)   + (5*c) 
>				+ (10*d)  + (20*e)  + (50*f)
>				+ (100*g) + (200*h)

> instance Ord (Combination) where
>	c1 <= c2 = combSum c1 <= combSum c2

> instance Num (Combination) where
> 	a + b = combzipWith (+) a b
>	a * b = combzipWith (*) a b
> 	a - b = combzipWith (-) a b
>	abs a = combMap (abs) a
>	signum a = 1
>	fromInteger x = C (x,x,x,x,x,x,x,x)

> data PopulationData = P { -- number of population still in circulation
>			    currPop :: Integer
>			    -- number of population which has 
>			  , currWin :: Integer
>			    -- current generation
>			  , currGen :: Integer} 
>			    -- actual generation information
>			  --, nxtGen :: [Combination]
>			  --, winner :: [Combination] }
>			deriving (Show, Eq, Ord)

> combFinder :: [Combination] -> [Combination] -> Integer -> [PopulationData]
> combFinder population wins currGen   
>	= (P genPop winnerPop currGen) -- newGen wins) 
>	: combFinder newGen (wins `union`  winners) (succ currGen)
>	where ((newGen, winners), (genPop, winnerPop))  = (id <#> (len))
>							$ fitfunc population 
>	      fitfunc l = filter2
>			$ filter1
>			$ nextGen
>			where filter2 = partition (\x -> (combSum x) /= 200) 
>			      filter1 = filter (\x -> (combSum x) <= 200)  
>			      nextGen = removeDupes 
>				      $ [p+q | p<-l, q<-combBasis, not (p `elem` wins)]
>	      len x = (fromIntegral (length x))::Integer


> combBasis :: [Combination]
> combBasis = [(C (0,0,0,0,0,0,0,1)), (C (0,0,0,0,0,0,1,0)), (C (0,0,0,0,0,1,0,0))
>	      ,(C (0,0,0,0,1,0,0,0)), (C (0,0,0,1,0,0,0,0)), (C (0,0,1,0,0,0,0,0))
>	      ,(C (0,1,0,0,0,0,0,0)), (C (1,0,0,0,0,0,0,0))]


> ppList 	:: Show a => [a] -> IO ()
> ppList [] 	= putStrLn "done"
> ppList (x:xs) = do
>	putStrLn (show x)
>	ppList xs



> main = do
> 	ppList $ combFinder combBasis [] 0

