> module P19.P19 where
> import PELib.PELib (divides)

===============================================================================
Problem 19
14 June 2002

You are given the following information, but you may prefer to do some research
for yourself.

    * 1 Jan 1900 was a Monday.
    * Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
    * A leap year occurs on any year evenly divisible by 4, but not on a century
    * unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1
Jan 1901 to 31 Dec 2000)?
===============================================================================


Heres what we do, we create an infinite list which simply counts off the name of
the day, (where 0 == Sunday, 1..6 = monday .. saturday) along with the year its
associated with.

 

> dayStream :: Integer -> Int -> Integer -> Integer -> Bool -> [(Int, Integer, Integer, Bool)]
> dayStream currDay currMonth currYear dayNum isFirst
>    | currMonth == 11        = today
>                             : (dayStream 0 0 (currYear+1) ((dayNum+1) `mod` 7) True)
>    | currDay == monthLen    = today
>                             : (dayStream 0 (currMonth+1) (currYear) ((dayNum+1) `mod` 7) True)
>    | otherwise              = today
>                             : (dayStream (currDay+1) (currMonth) (currYear) ((dayNum + 1) `mod` 7) False)
>    where
>         monthListNorm = [31,28,31,30,31,30,31,31,30,31,30,31]
>         monthListLeap = [31,29,30,31,30,31,30,30,31,30,30,31]
>         isLeap = ((4 `divides` currYear) && ((not (100 `divides` currYear)) || (400 `divides` currYear)))
>         monthLen = if isLeap 
>                    then (monthListLeap !! currMonth)
>                    else (monthListNorm !! currMonth)
>         maxDays = if isLeap
>                   then 365
>                   else 364          
>         today = (currMonth, dayNum, currYear, isFirst)                       


Now that we have our infinite list of days, lets create a stream starting at the
given date

> initStream :: [(Int, Integer, Integer, Bool)]
> initStream = dayStream 0 0 1900 1 True

Now that we have that, lets create a filter function which will select all
sundays which fall on the first of the month.

> streamFilter :: (Int, Integer, Integer, Bool) -> Bool
> streamFilter (_,dayNum,_,isFirst) = (isFirst && (dayNum == 0))

Now we just need to take only those dates before Dec 31, 2000 (that is, up until
jan 1, 2001)

> dateList :: [(Int, Integer, Integer, Bool)]
> dateList = takeWhile (\(_,_,y,_) -> y /= 2001) initStream

now lets filter the list and take its length to solve P19

> solvenineteen :: Int
> solvenineteen = length $ filter streamFilter dateList




