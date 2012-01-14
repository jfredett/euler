> module P31 where

===============================================================================
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1*£1 + *50p + 2*20p + 1*5p + 1*2p + 3*1p

How many different ways can £2 be made usin any number of coins?
===============================================================================

> import PELib.PELib ((<#>), (><), crossWith)
> import Data.Set ( fromList
>		  , Set)
> import qualified Data.Set as S

So, there is an easy way to solve this, but there is also a cool way.

A Genetic(ish) way.

Really, this is more like "boiling out" the solution, like you would boil out the impurities from
a metal. I'll explain how it works in a moment. First, lets look at some theory about the
problem.

We are asked to find the number of distinct solutions to the following equation in the variable 
p:

f = a (1p) + a (2p) + a (5p) + a (10p) + a (20p) + a (50p) + a (100p) + a (200p) == 200
     1        2        3        4	  5         6         7          8

This, on it's face, looks hard, but thats only because we have to deal with finding coefficients
that give valid solutions to the function, but really we could equally well view this as finding
variables to a given multinomial equation in the variables a_n. 

the variables a_1,a_2 ... constitute a vector space in n-dimensions, and the equation, f(v), that
we're given forms a metric as follows:

d(v,u) = |f(v) - f(u)|

Properties of a metric: (from Wikipedia)

d(v,u) >= 0, (non-negativity). 
	Follows immediately from the absolute value sign

d(v,u) = 0 => v == u. (indentity of indistinguishable elements)
	Assume v /= u, that means the elements of v,u can't all be equal.
	d(v,u)  = <a,b,c,d,e,f,g,h> - <s,t,u,v,w,x,y,z> 
		= <a - s, b - t, c - u, d - v, e - w, f - x, g - y, h - z> = <0,0,0,0,0,0,0,0>
	       => a - s = 0, ...
	       => a = s, ... 
	       -) v /= u (- Therefore v == u if d(v,u) = 0

d(v,u) = d(u,v) (symmetry)
	Follows from Absolute value sign, since  f x is a number, and x - y = - (y - x), then 
	 the absolute value sign removes the negative.

d(x,z) <= d(x,y) + d(y,z) (triangle inequality)
	assume d(x,z) > d(x,y) + d(y,z) =>
	|f x - f z| > |f x - f y| + |f y - f z| =>
	let's assume each of the d(x,z), etc are greater than zero w/o the absolute value signs
	 the other cases are left to the reader.
	
	f x - f z > f x - f y + f y - f z =>
	f x - f z > f x - f z 
	let a = f x - f z
	a == a  :: reflexivity
	a > a :: shown
	-) a == a, a > a (- by the trichotomy law.

	Therefore, the triangle property holds for d(v,u) = |f v - f u|

So now we have a Vector Space with a Metric d(x,y). Giving us a Metric space. 

We then find that the question is really, 

how many vectors, x, with integer coefficients satisfy:

d(x,0) = 200

Well thats an interesting reduction, we're now effectively searching for points on an 8-dimensional "sphere" that are integral, ideally, we could find a simple equation for this discrete sphere.
But thats a pain, it's much more fun to do it in a genetic-ish fashion. 

Lets talk about vector spaces. In a VS, there exist sets of vectors that span the VS. Ideally, we
have a least-most linearly independent set, called a basis. Now, lets pretend we have this basis 
(apparently, this fact requires the unfortunately nasty, but highly useful "Axiom of Choice", but
that is a topic for another day.) what does it mean to span the set? Basically, it means that a
set of vectors with this property can, by linear combination and scaling, generate the whole 
vector space. In our case, we're considering scaling only by integers.

So, with that background we can describe our algorithm, which is general enough to encompass
any problem of the form:

d(x,0) = y, for any integer y


First we start with a definition of three things

Vectors in the space "combination"
The basis set of the space "combBasis"
the distance function "combSum"


We can create the first one fairly easily it's just a list of Integers, our VS will be over Z,Z, 
for efficiency, we'll store the combSum of the vector with the vector itself, we need the vector to be able to distinguish between two winner cases with different vectors, but having it stored
will save us the cost of computing it repeatedly.


> data Combination = C 	{ -- the vector itself
>			  vector :: ![Integer]
>			, -- d(x,0)
>			  len :: !Integer
>		      	}	 
>		      deriving (Eq,Show)

This gives us a general interface to use when we want to calculate d(x,0), if you want to make 
you're own, just fiddle with the default "combSum" or "denominations"

> coinDenominations = [1,2,5,10,20,50,100,200]

> class Num a => Dist a where
>	dist :: a -> Integer 
> instance Dist Combination where
>	dist = generalCombSum coinDenominations
> instance Ord Combination where
>	compare (C v l) (C v' l') 
>		| l == l' 	= compare v v' 
>		| otherwise	= compare l l'


> -- nice, cheaply bought hack,...
> mkComb x = (C x x') where x' = dist (C x 0)

We'll make a type synonym for convienence, it's really just like a #define, but only for the 
 type system.

> type Combinations = [Combination]
> type CombinationSet = Set Combination

this is the real meat of the operations we'll need, the Num type class will allow us to mess around with our vectors with relative impunity, note that signum and fromInteger are basically 
broken, we won't use them much anyway.


> instance Num (Combination) where
> 	(C v l) + (C v' l') = (C (zipWith (+) v v') (l + l')) 
>	(C v l) - (C v' l') = (C (zipWith (-) v v') (l + l'))
>	(C v l) * (C v' l') = (C (zipWith (*) v v') (l + l'))
>	abs (C v l) 	    = (C (map abs v) (abs l))
>	-- these are basically broken
>	signum (C _ l)	    = (C [] 0)
>	fromInteger x	    = (C [] 0) 

Now we can write the d(x,y) analog, which is pretty simple, we just want to create a polynomial.

> generalCombSum ::  [Integer] -> Combination -> Integer
> generalCombSum values (C ls _) = sum 
>				 $ zipWith (*) ls values

This data structure is just for holding generational information.

> data PopulationData = P { -- number of population still in circulation
>			    currPop :: Integer
>			    -- number of population which has 
>			  , currWin :: Integer
>			    -- current generation
>			  , currGen :: Integer} 
>			deriving (Show, Eq, Ord)

This is the meat of the operation, the algorithm is fairly simple.

We'll talk about it in terms of populations

we have a population of vectors that form a basis for the vector space to search
we also have a set of vectors which tell us which "directions" to search in,
we also have a target number (called limit) which correspond to the "y" parameter in the normal
problem
we also have a "winners" set, which contains a copy of all the winning values so far.
we have a "generation count" just for record keeping
and finally, we have the "combSum" function, which corresponds to d(x,0) 

We start by checking our current population for any "dead" specifically, we just want to get rid  of them, so we filter the population for elements which satisfy the condition d(x,0) <= y
Next, we remove any duplicate elements, including any duplicate winners (eg we take the 
	difference of the "winners" set with the generation set)
Next, we want to isolate the elements which are "winners." meaning that they satisfy d(x,0) == y. we save these in a set along with the number of winners in that generation
Next, we take the rest of the set, and add across each element of the "directions" set, giving us the next generation.
Finally, we put the current number of winners, the current population size, and the current 
 generation number, in the "populationData" type, attach it to the end of the generated list, 
 and move on to the next case with the new population, the new set of winners and the next genera tion, holding the rest constant. Thus we generate a non-necessarily finite list tracking the 
 "population" of solutions until we exhaust the search space.

The number of winners at the end is the answer we want. 

> scFinder :: CombinationSet -> CombinationSet -> CombinationSet -> Integer -> Integer
>	   -> [PopulationData]
> scFinder curPop curWinners basis curGen limit
>	= (P genPop winnerPop curGen)
>	: if (genPop == 0) && (curGen /= 0)
>	then []
>	else (scFinder newGen newWinners basis (curGen+1) limit)
>	where newWinners = S.union curWinners winners
>	      winnerPop  = ((size winners) + (size curWinners))
>	      ((newGen, winners), (genPop, winPop)) 	= (id <#> size) 
>							$ fitFunc curPop

>			-- this is also a nasty slow bit, linearity makes me cringe maybe 
>			-- another parition w/ splitpoint = [199], and then 
>	      fitFunc s = (S.split splitPointLo) -- S.partition (\x -> (len x) /= 200)
>			$ (fst . S.split splitPointHi)
>			$ S.difference 
>				-- this is where alot of the slowdown is, we have a 
>				-- O(nm*log n) operation followed by a O(n log n) op
>				-- this gives a total of O(n log n * log log n)
>				-- is there any way to make it faster? (beside paralellism?)
>				(S.map (uncurry (+)) (s >< basis)) -- (crossWith (+) s basis)
>				curWinners
>	      splitPointHi = (C (repeat 1000) 200)
>	      splitPointLo = (C [] 200)


> testSC = scFinder (fromList basis) (S.empty) (fromList basis) 0 200 
>	where basis = genCombBasis coinDenominations

> size :: Set a -> Integer
> size s = (fromIntegral (S.size s))

This generates the basis of the vector space, given the set of values

> genCombBasis :: [Integer] -> Combinations
> genCombBasis ls = [ mkComb ((replicate l 0) ++ [1] ++ (replicate ((length ls) - (l + 1)) 0)) 
>		    | l <- [0..((length ls) - 1)]] 

this is a default basis 

> -- currently being stupidly used globally in the combFinder function, which will be fixed when
> -- combFinder is rewritten to come up with this stuff on it's own.
> combBasis :: Combinations
> combBasis = genCombBasis [1,2,5,10,20,50,100,200]

pretty printing for lists

> ppList 	:: Show a => [a] -> IO ()
> ppList [] 	= putStrLn "done"
> ppList (x:xs) = do
>	putStrLn (show x)
>	ppList xs

and the main function, which will solve our problem

> main = do
> 	-- ppList $ defaultFinder combBasis (generalCombSum [1,2,5,10,20,50,100,200]) 200
>	-- the original version took :

real    139m23.457s
user    137m27.969s
sys     0m11.059s

> 	-- this is the set version:
>	ppList $ scFinder (fromList $ genCombBasis coinDenominations) 
>			  (S.empty) 
>			  (S.deleteMax . fromList $ genCombBasis coinDenominations) 
>			  0 200 

This is down to about 5 minutes (5:00.064 or somesuch)


here, since we need to compile to get good speed, we write "solvethirtyone" to print a friendly message with the appropriate compile sequence to use"


> solvethirtyone = do
>	putStrLn "P31 needs to be compiled, because the solution is fairly machine intensive"
>	putStrLn "The appropriate command to use is as follows:"
>	putStrLn ""
>	putStrLn ghc_cmd
>	putStrLn ""

	putStrLn "Hell, we'll compile and run it for you, right from the interpreter"
	system ghc_cmd
	system "time ./test"
	system "rm test"

>	where ghc_cmd = "ghc --make -O2 -optc -O3 -o test P31/P31.lhs -main-is P31 "
>                    ++ "-i../PELib/ -fforce-recomp -funbox-strict-fields "
>                    ++ "-funfolding-use-threshold=256 -fvia-c"



Optimizations
	Parallelize?
	
	
