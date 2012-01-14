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


We can create the first one fairly easily it's just a list of Integers, our VS will be over Z,Z

> newtype Combination = C [Integer] 
>			deriving (Eq,Ord,Show)

We'll make a type synonym for convienence, it's really just like a #define, but only for the 
 type system.

> type Combinations = [Combination]

we need to lift some normal list operations to the combination's level.

> combMap :: (Integer -> Integer) -> Combination -> Combination
> combMap f (C ls) = C (map f ls)
> combZipWith :: (Integer -> Integer -> Integer) -> Combination -> Combination -> Combination
> combZipWith f (C as) (C bs) = (C (zipWith f as bs))

this is the real meat of the operations we'll need, the Num type class will allow us to mess around with our vectors with relative impunity, note that signum and fromInteger are basically 
broken, we won't use them much anyway.

> instance Num (Combination) where
> 	a + b = combZipWith (+) a b
>	a * b = combZipWith (*) a b
> 	a - b = combZipWith (-) a b
>	abs a = combMap (abs) a
>	signum a = 1
>	fromInteger x = C [x]

Now we can write the d(x,y) analog, which is pretty simple, we just want to create a polynomial.

> generalCombSum ::  [Integer] -> Combination -> Integer
> generalCombSum values (C ls) 	= sum 
>				$ zipWith (*) ls values

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

> combFinder :: Combinations -> Combinations -> Integer -> Integer 
>	     -> (Combination -> Integer) ->[PopulationData]
> combFinder population wins currGen limit combSum
>	= (P genPop (winnerPop + (fromIntegral (length wins))) currGen)
>	: combFinder newGen (wins `union`  winners) (succ currGen) limit combSum
>	where newWinnerPop = (winnerPop + (fromIntegral . length) wins)
>	      ((newGen, winners), (genPop, winnerPop))  = (id <#> (len))
>							$ fitfunc population 
>	      fitfunc l = filter2
>			$ removeDupes
>			$ filter1
>			$ nextGen
>			where filter2 = partition (\x -> (combSum x) /= limit) 
>			      filter1 = filter (\x -> (combSum x) <= limit)  
>			      nextGen = removeDupes 
>				      $ [p+q | p <- l, q <- combBasis, not (p `elem` wins)]
>	      len x = (fromIntegral (length x))::Integer

This generates the basis of the vector space, given the set of values

> genCombBasis :: [Integer] -> Combinations
> genCombBasis ls = [ C ((replicate l 0) ++ [1] ++ (replicate ((length ls) - (l + 1)) 0)) 
>		    | l <- [0..((length ls) - 1)]] 

this is a default basis 

> -- currently being stupidly used globally in the combFinder function, which will be fixed when
> -- combFinder is rewritten to come up with this stuff on it's own.
> combBasis :: Combinations
> combBasis = genCombBasis [1,2,5,10,20,50,100,200]

a quick little default version of combFinder

> defaultFinder basis combSum limit = combFinder basis [] 0 limit combSum

pretty printing for lists

> ppList 	:: Show a => [a] -> IO ()
> ppList [] 	= putStrLn "done"
> ppList (x:xs) = do
>	putStrLn (show x)
>	ppList xs

and the main function, which will solve our problem

> main = do
> 	ppList $ defaultFinder (init combBasis) (generalCombSum [1,2,5,10,20,50,100,200]) 200

here, since we need to compile to get good speed, we write "solvethirtyone" to print a friendly message with the appropriate compile sequence to use"

> solvethirtyone = do
>	putStrLn "P31 needs to be compiled, because the solution is fairly machine intensive"
>	putStrLn "The appropriate command to use is as follows:"
>	putStrLn ""
> 	putStrLn ""



Optimizations
	Use Data.Set, and split instead of filter, it'll reduce the fitFunc time from O(n) to
	 O(log n), it'll also remove the need for removeDupes everywhere. It'll also give us
	 faster union (O(n + m) as opposed to O(n^2)) and O(1) "length" (in the form of size).
	Parallelize?
	
	
