/*
  Large non-Mersenne prime
  Problem 97

  The first known prime found to exceed one million digits was discovered in 1999,
  and is a Mersenne prime of the form 2^6972593−1; it contains exactly 2,098,960 digits.
  Subsequently other Mersenne primes, of the form 2^p−1, have been found which contain more digits.

  However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits:
  28433 × 2^7830457 + 1.

  Find the last ten digits of this prime number.
*/

open Big_int;
open Utils;

/*
  1. tried to actually compute the value, just for the LOLs. 30 mins to execute...

     bi_power (big_int_of_int 2) (big_int_of_int 7830457) |> mult_big_int (big_int_of_int 28433);

  2. second approach takes 2s to compute. a slight improvement I would say.
*/

let ten_bil = big_int_of_int 10_000_000_000;
let result = ref (unit_big_int);

/* we calculate the power iteratively while keeping only the last 10 digits */
for i in 1 to 7830457 {
  result := mod_big_int (shift_left_big_int !result 1) ten_bil;
};

(add_big_int (mod_big_int (mult_big_int (big_int_of_int 28433) !result) ten_bil) unit_big_int) |> string_of_big_int |> print_endline;
