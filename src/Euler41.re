/*
  Pandigital prime
  Problem 41

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
  For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?
*/

open Util;

/* start with a brute force approach to check if it works.
   with limit = 1_000_000_000 takes ~3min to run... ouch!
   we can lower the limit to 100_000_000, which takes only 8s.
 */

let limit = 100_000_000;

let is_pandigital n => {
  let rec check_digits l i => {
    switch l {
      | [] => { true }
      | [hd, ...tl] => { (hd == i) && (check_digits tl (i+1)) }
    }
  };
  let digs = (List.sort compare (digits n));
  check_digits digs 1;
};

let primes = sieve limit;

let largest = ref 0;

let n = ref (limit-1);

while (not (primes.(!n) && (is_pandigital !n))) {
  n := !n - 1;
};

print_int !n;
