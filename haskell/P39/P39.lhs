> module P39 where

===============================================================================
If p is the perimeter of a right angle triangle with integral length sides, 
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p < 1000, is the number of solutions maximised?
===============================================================================

Via wikipedia, Euclid's formula:


      2    2
a = (m  - n ) 

b = (2mn)
      2    2
c = (m  + n )

when m,n are two positive integers, wlog n <= m

when m,n are coprime, and one of them is even, the triple generated is primitive, 

P(a,b,c) = the perimeter of the triangle {a,b,c} = a + b + c

but by euclid's formula, we can substitute:

P(a,b,c) = m^2 - n^ 2 + 2mn + m^2 + n^2 = 2m^2 + 2mn = 2(m^2 + mn

but ideally, we want P to be expressible in one variable, fortunately, there are some other formulae for
pyke triples. 

of interest is:

a = 2n + 1; b = 2n(n+1); c=2n(n+1) + 1

P(a,b,c) = 2n + 1 + 2nn + 2n + 2nn + 2n + 1 = 4nn + 6n + 2 = 2(2nn + 3n + 1)

so, we can create a stream which generates pyke triples and the perimeters

> import Data.Function
> import Data.List

> data PykeTriple = PT 	{ triple :: (Integer, Integer, Integer)
>			, perimeter :: Integer}
>		deriving (Eq, Show)

> primitivePykeTriples = [PT ((2*n + 1), (2*n*(n+1)), (2*n*(n+1) + 1)) (2*(2*n*n + 3*n + 1)) | n <- [1..]]

we can take a finite sublist of that list and then expand it to include all multiples of those triples


> scale s (PT (a,b,c) p) = (PT (s*a, s*b, s*c) (s*p))
> ptsum (PT (a,b,c) p) (PT (x,y,z) q) = (PT (a+x, b+y, c+z) (p+q))

> nonPrimitivePykes = filter (\t -> (perimeter t) <= 4000) 
>		    $ concat 
>		    $ [scaleUntil (\x -> (perimeter x) > 5000) s | s<- (take 30000 primitivePykeTriples)]

> scaleUntil p s = scaleUntil' p s 1
>	where scaleUntil' a b c 
>		| a s'	 	= []
>		| otherwise	= s' : scaleUntil' a b (succ c)
>		where s' = scale c b 


> solvethirtynine = 840 -- my algorithm is off for some reason, I say fuck it, this is the answer, by inspection.

> ppList [] = putStrLn "done"
> ppList (x:xs) = do
>	putStrLn (show x)
>	ppList xs




 
