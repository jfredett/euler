> module P188 where


===============================================================================
The hyperexponentiation or tetration of a number a by a positive integer b, denoted by a***b, is recursively defined by:

a***1 = a,
a***(k+1) = a(a***k).

Thus we have e.g. 3***2 = 3^3 = 27, hence 3***3 = 3***27 = 7625597484987 and 3***4 is roughly 103.6383346400240996*10^12.

Find the last 8 digits of 1777***1855.
===============================================================================

Euler's Theorem:

a^phi(n) =~ 1 mod n 

this means that when we try to calculate 


a^x...  mod n

where x = a^a^...

we really just need to calculate:


a^(x mod phi(n)) mod n

which can then be recursively applied, since x = a^(x') we get

a^(a^(x' mod (phi(phi(n)))) mod phi(n)) and so forth.


further, since phi(n) < n forall n (proof below), we only need to calculate till we hit phi(n) = 1, at which point we'll have the following result


phi_k(n) = phi(phi(... k times ...(n))...)


phi_x(n) = 1

a^...^a mod phi_x(n) =
a^...^a mod 1 =
a^...^0 =
a^...^1 

after we reach phi_x(n), we don't need to continue anymore.

So, using the wonderful grfa from pelib, we can write this modular tetrater quickly


