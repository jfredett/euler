> module GRFA where
> import Test.QuickCheck

GRFA: The General Repeat/Constrain Function Appliication algorithm

eg, generalized modular exponentiation



Really, what we're doing here is applying two different functions, one turns a hard problem to an easy one, in this case, modular arithmetic
and another function which turns and easy problem to a hard one, in this case exponentiation.

We call these two transforms the simple and the complex transforms. 

>-- the repeater function takes a complex transform, a simplifying transform, and an initial value, and returns a simple result.
> repeater :: (a -> b) -> (b -> a) -> a -> a
> repeater c s i = (s . c) i 

now we can build the actual grfa:

the grfa will need a halting condition, we're aiming to accept functions of the form:

((a `comp` b) `simp` c) -> (((a `op` a) `comp` (reduce b)) `simp` c)  

so comp is a binary function

comp :: a -> b -> a

which looks like this:

comp a b = if (endcondition b) 
	 then (a `op` a) `comp` (reduce b) 
	 else (a)
which means we need to provide the `op`, endcondition, and reduce functions, so the final type signature of comp is:

     
 type Comp 	=  (a -> a -> a) 		-- `op` sig
  		-> (b -> b) 			-- reduce sig
     		-> (b -> Bool) 			-- endcondition sig
		-> a -> b 			-- inputs
     		-> a 				-- return type

simp, on the other hand, needs a much simpler signature, it is simply a binary operation and an initial value (of type c):

 type Simp 	=  (a -> c -> a) -> c -> a

so the grfa is a comp and a simp and some initial inputs, ie:

So lets build grfa, 


> grfa 	:: (a -> a -> a) 
>      	-> (b -> b)
>     	-> (b -> Bool)
> 	-> (a -> c -> a) 
> 	-> a -> b -> c 
> 	-> a
> grfa op reduce endcond simplify a b c = grfawork a b a
> 	where
> 	  grfawork a b origA 
> 		| (endcond . reduce) b 	 = simplify (a `op` origA) c
> 		| otherwise 		 = grfawork (simplify (a `op` origA) c) (reduce b) origA  

now lets give that a whirl with a simple test, we'll use quickcheck to ensure that the following operations work when you use thier repeated 
counterparts.

> grfa_Addition :: Int -> Int -> Int -> Property
> grfa_Addition x y k = ((k > 0) && (x >= 0) && (y >= 0)) 
> 		    ==> ((x + y) `mod` k) == (grfa (\x _ -> succ x) (pred) (==0) (mod) x y k)
>

> grfa_Multiplication :: Int -> Int -> Int -> Property
> grfa_Multiplication x y k = ((x > 0) && (x >= 0) && (y >= 0))
> 			  ==> ((x * y) `mod` k) == (grfa (+) (pred) (==0) (mod) x y k)

you'll notice, these are as slow as cold molasses going uphill during a blizzard. That's because we aren't taking advantage of the "repeated squaring" analog.

realize that what we're doing here is basically the computation of the following chain:

constrainWith c (f . f . f . f ... f x)

more accurately, we constrain at every step f, so really we're building the following chain:

f' = (constrainWith c f)
f' . f' . f' ... f' x

we build this chain linearly, adding one link at a time, but why be linear? At step 1, we've constructed the chain 'f', at step two, we've constructed 
f . f, why not double this chain over itself, so that at step 3, we can have f . f . f . f, etc?

the algorithm is familar, we usually call it repeated squaring, that is, calculating a large exponential, like

a^32

as a series of simple exponentials

((((a^2)^2)^2)^2)

which is easy to calculate, since at each stage we're really just squaring what we already have.

so, new algorithm GRSFA : Generalized Repeated Constrained Squaring Function Application algorithm :)

> grsfa :: (a -> a -> a) 
>      	-> (b -> b) -> (b -> b) -- we need two reduction functions, one for the times we can use the squaring technique, and one normal one for when 
> 				-- we cant
> 	-> (b -> Bool) 		-- this is the "differentiator", which tells us which of the two reduction functions to use, true for linear, false
> 				-- otherwise
>     	-> (b -> Bool)
> 	-> (a -> c -> a) 
> 	-> a -> b -> c 
> 	-> a
> grsfa op reduce_linear reduce_half differ endcond simplify a b c = grfa op reduce' endcond simplify a b c
> 	where reduce' x 
> 		| differ x = reduce_linear b
> 		| otherwise x = reduce_half b

Now we can test this, with modular exponentiation, as follows:

> grsfa_ModExp = 
> 	where
> 		op 	
> 
> 
