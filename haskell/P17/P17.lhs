> module P17.P17 where
> import qualified Data.Map as Dict
> import Test.QuickCheck

===============================================================================
Problem 17
17 May 2002

If the numbers 1 to 5 are written out in words: one, two, three, four, five;
there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters.
===============================================================================

NOTES: 
word-number is the english word for a number

First, lets note that for all numbers strictly less than ten, we need to count
the length off word-number, so lets set up some dictionary rules which map a
word-number to its string length, we'll also mark off all the *teens and also
every increment of ten up to 100. Note that zero has length 0, this will prevent
it from showing up in the parsing of "20" or "1024"

> dict :: Dict.Map Integer String
> dictList = [
>             (0,  ""),         (1,  "one"),       (2,  "two"),
>             (3,  "three"),    (4,  "four"),      (5,  "five"),      
>             (6,  "six"),      (7,  "seven"),     (8,  "eight"),
>             (9,  "nine"),     (10, "ten"),       (11, "eleven"),
>             (12, "twelve"),   (13, "thirteen"),  (14, "fourteen"),
>             (15, "fifteen"),  (16, "sixteen"),   (17, "seventeen"),
>             (18, "eighteen"), (19, "nineteen"),  (20, "twenty"), 
>             (30, "thirty"),   (40, "forty"),     (50, "fifty"),
>             (60, "sixty"),    (70, "seventy"),   (80, "eighty"), 
>             (90, "ninety")
>            ]
> dict = Dict.fromList dictList

Now we'll create a couple of functions to return appropriate values out of the
dict.

> getWordNumber :: Integer -> String
> getWordNumber i = Dict.findWithDefault "" i dict

> getWordLength :: Integer -> Int
> getWordLength i = length (getWordNumber i)


So now we need a function Integer -> String, which takes a number from the form:
"1234" and returns "one thousand, two hundred, and thirty four" 

> numToWord :: Integer -> String
> numToWord i 
>    -- first, lets see if less than 20, if so, we can do a straight lookup
>    | i < 20       = getWordNumber i
>    -- now, lets check to see if it's 1000, if so, we've hit the max.
>    | i == 1000    = "one thousand"
>    -- next, we check to see if it is less than 100, if so, we'll pass it down
>    -- the line to a handler function.
>    | i < 100      = smallNumHandler i
>    -- otherwise, send it to the other handler
>    | i >= 100     = bigNumHandler i
>    -- otherwise, fail.
>    | otherwise    = ""
>    where
>         smallNumHandler :: Integer -> String
>         smallNumHandler x =
>              let (d,m) = x `divMod` 10 in
>                  (getWordNumber (d*10)) ++ " " ++ (getWordNumber m)
>         bigNumHandler :: Integer -> String
>         bigNumHandler x =
>              let (d,m) = x `divMod` 100 in
>                   (getWordNumber d) ++ " hundred and " ++ (numToWord m)
              
Granted, we don't actually need to write the above, we actually need to write
numToWordLength, which is similar, but uses getWordLength rather than
getWordNumber. so here it is, rewritten.

> numToWordLength :: Integer -> Int
> numToWordLength i  
>    -- first, lets see if less than 20, if so, we can do a straight lookup
>    | i < 20       = getWordLength i
>    -- now, lets check to see if it's 1000, if so, we've hit the max.
>    | i == 1000    = 11
>    -- next, we check to see if it is less than 100, if so, we'll pass it down
>    -- the line to a handler function.
>    | i < 100      = smallNumHandler i
>    -- otherwise, send it to the other handler
>    | i >= 100     = bigNumHandler i
>    -- otherwise, fail.
>    | otherwise    = 0
>    where
>         smallNumHandler :: Integer -> Int
>         smallNumHandler x =
>              let (d,m) = x `divMod` 10 in
>                  (getWordLength (d*10)) + (getWordLength m)
>         bigNumHandler :: Integer -> Int
>         bigNumHandler x 
>                   | m == 0       = (getWordLength d) + 7
>                   | otherwise    = (getWordLength d) + 10 + (numToWordLength m)
>                   where (d,m) = x `divMod` 100


the magic 11 on line 90, 0 on line 97, and 10 on 106 are just the lengths of the
corresponding strings in the other function.

So then solveseventeen is simply:

> solveseventeen :: Int
> solveseventeen = foldr ((+) . numToWordLength) 0 [1..1000]

