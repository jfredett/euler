> module P22.P22 where

===============================================================================
Problem 22
19 July 2002

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
containing over five-thousand first names, begin by sorting it into alphabetical
order. Then working out the alphabetical value for each name, multiply this
value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 53 = 49714.

What is the total of all the name scores in the file?
===============================================================================

We've already put all of those names in a file called "Names.lhs", lets import
it.

> import P22.Names
> --we'll need these later
> import Data.Char
> import Data.List

This contains a sole variable called "names"

We'll need to sort it, thats simple, we'll also need to zip it with a position
value, so we'll just zip [1..]

> names' :: [([Char], Integer)]
> names' = zip (sort names) [1..]



Lets now create a way to turn each letter into a number, 

Now lets turn that to a list of lists of ints by mapping a "toIdx" function,
which is simply a modified ord function which will map A -> 1 and Z->26, and
correponding values in between

> toIdx :: Char -> Int
> toIdx c = (ord c) - 64

Now lets just create a "nameSum" function which maps toIdx across a list, and
returns the sum * the position in the list.

> nameSum :: ([Char], Integer) -> Integer
> nameSum (cs, place) = place * sum [(fromIntegral . toIdx) x | x <- cs] 

Finally, lets map that across the big list, and then take it's sum

> solvetwentytwo :: Integer
> solvetwentytwo = sum [nameSum x | x <- names']

That is all.
