> module P18.P18 where
> import Random
> import System.IO.Unsafe

===============================================================================
Problem 18
31 May 2002

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

3
7 5
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by
trying every route. However, Problem 67, is the same challenge with a triangle
containing one-hundred rows; it cannot be solved by brute force, and requires a
clever method! ;o)
===============================================================================

We're going to represent this structure as a tree like list, then we'll use a greedy
"choose big first" algorithm to walk the tree and solve it.

> treeList :: [[Integer]]
> treeList = [
>                                         [75],
>                                       [95, 64],
>                                     [17, 47, 82],
>                                   [18, 35, 87, 10],
>                                 [20, 04, 82, 47, 65],
>                               [19, 01, 23, 75, 03, 34],
>                             [88, 02, 77, 73, 07, 63, 67],
>                           [99, 65, 04, 28, 06, 16, 70, 92],
>                         [41, 41, 26, 56, 83, 40, 80, 70, 33],
>                       [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
>                     [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
>                   [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
>                 [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
>               [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
>             [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
>            ]

for testing, we'll include the other treeList

> tinyTreeList :: [[Integer]]
> tinyTreeList = [
>                       [3],
>                     [7 , 5],
>                   [2,  4,  6],
>                 [8 , 05 , 9 , 3]
>                ]

> tinyTreeList2 :: [[Integer]]
> tinyTreeList2 = [
>                         [1],
>                       [2,  3],
>                     [9,  0, 0],
>                   [9,  0,  0, 0]
>                 ]

What we need to do is walk down this tree and keep track of our position k in
list-n, we'll then choose either element k or k+1 in list-n+1. We'll to this
till we hit the end of the list. Easy as 3.14159...

We'll have walk tree just return the path chosen according to the "chooser" function
which will take two integers and return one of them, and the position it was
found at 

> walkTree :: [[Integer]] -> (Integer -> Integer -> Integer) -> [Integer]
> walkTree (lol) choose = walkTreeHelp lol 0 choose 

> walkTreeHelp :: [[Integer]] -> Int -> (Integer -> Integer -> Integer) -> [Integer]  
> walkTreeHelp lol@(x:xs) lastPos choose 
>    -- if we have no lists left, we'll bugger out
>    | null lol          = []
>    | (null xs)         = [choose (x !! lastPos) (x !! (lastPos+1))]
>    -- next, if the length of our list is 1, we'll pass its only element on
>    | length x == 1     = x ++ (walkTreeHelp xs 0 choose)
>    -- now, if our list is of length 2 or more, we'll take elements lastPos and
>    -- lastPos+1, and choose between them with the choose function. then, we'll
>    -- move ahead accordingly.
>    | otherwise         = chooseNextPath x 
>    where 
>         chooseNextPath :: [Integer] -> [Integer]
>         chooseNextPath es = 
>              let val = choose (es !! lastPos) (es !! (lastPos+1)) 
>                  np = if (es !! lastPos) == val then lastPos else (lastPos+1) in
>                  val : (walkTreeHelp xs np choose)
>              

Since this only works in the optimal case, we'll make a nondeterministic "choose
max" function, which uses a random list chooser function, and will choose to walk down
the tree randomly. However, we'll make the function lean (about 70 percent)
toward the bigger side

don't be decieved, this bad boy is DETERMINISTIC!

> chooseRandElem :: [Integer] -> Integer
> chooseRandElem ls = 
>    let len = (length ls) - 1 
>        currGen = mkStdGen
>        rands = head (randomRs (1,len) (unsafePerformIO getStdGen)) in
>        ls !! rands

we can now create a semi-random chooser function using the above

> chooser :: Integer -> Integer -> Integer 
> chooser a b =
>    let x = max a b 
>        y = min a b in
>        chooseRandElem [x,y] 

to solve, lets simply build a list of repeated walkTree sums, and then take the
max of all of those

We know there are about 16000 of them (see problem description) so we'll run it
about 4000 times to ensure we get a good sample (25 probability we'll find the
max.)

> multiWalk :: Integer -> [Integer]
> multiWalk 0 = []
> multiWalk times = (sum $ walkTree treeList chooser) : multiWalk (times-1)

We soon find that this-- doesn't work... 

We are sad. New Strategy time!

Hmm, lets look at the structure of this tree. We notice that at every point,
we're really only adding one number to the number in the same position below it,
as well as the one after it. We could- theoretically, fold up the tree in this
way. Then we'd just need to take the maximum of the resulting list-- The
question though- is this any better than trying to walk down each path?

Lets think. 

For each element of a list, we can add it to the elements of the next list in
constant time. We also know that for each list, we have k_n elements to add to
the list below it. So we have k_n * O(1) per list, for n lists, or

n
--      n(n+1)      2
\  k  = -----  = O(n )
/_  i     2
i=0  

Now lets look at the brute force case

At step 1, we have 1 choice, so we choose the only element of k_1, we then have
2 choices, we continue to have 2 choices n times, so its obvious that means we
have 2^n choices. So Folding the tree looks like a good method!

Lets think about how to do this.

Imperatively, we might write it like this

for 0 < j < n
     for 0 < i < len(k_j) 
          x = k_j[i]
          m = j+1
          k_n[i] += x
          k_n[i+1] += x

so lets do some functionalizing, we'll generalize to take any binary function

 foldTreeList :: [[Integer]] -> (Integer -> Integer -> Integer) -> [Integer]
 foldTreeList [] _ = []
 foldTreeList [x] _ = x
 foldTreeList (x:y:xs) combine =
 
to be clear, we're trying to do the following (using tinyTree as an example)

 
        3
       / \
      7   5                    10   8            12  (14,12)  14
     / \ / \        ---\      /  \ / \    ---\  /  \   / \   /  \   ---\ 
    2   4   6       ---/     2    4   6   ---/ 8     5     9     3  ---/
   / \ / \ / \              /  \ / \ / \
  8   5   9   3            8    5   9   3 


 20 (17,19,17) (23,21,23) 17,

Lets note something- at any point where we we have a 2 tuple like that- we can
simply pick the bigger of the two.

AHA! Heres an idea

lets try walking UP the tree! instead of down! The algorithm looks like this:


maximum $ take 2 (last list) + (head (secondtolast list)) 

rinse and repeat on tail (last list) and tail (secondtolast list), this will
roll up the tree like this:

   3
  7 5    --\       3         --\     3     --\   23
 2 4 6   --/    7     5      --/   20  20  --/   
8 5 9 3      10   13    15  

which is, of course, the longest path in the list!

lets go for it

> rollUpTree :: [[Integer]] -> Integer
> rollUpTree [] = 0
> rollUpTree [[x]] = x
> rollUpTree (l1:l2:ls) =
>    rollUpTree ((rollUp l1 l2) : ls)

> rollUp :: [Integer] -> [Integer] -> [Integer]
> rollUp [] _ = []
> rollUp _ [] = []
> rollUp [x] [y] = [x+y]
> rollUp (x1:x2:xs) (y:ys) = ((max x1 x2) + y) : (rollUp (x2:xs) ys)


It works! (test it for your self) so solveeighteen is simply:

> solveeighteen :: Integer
> solveeighteen = rollUpTree (reverse treeList)

Done!
