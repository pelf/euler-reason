/*
  Pandigital prime
  Problem 41

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
  For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?
*/

open Utils;

/* 1. Started with a brute force approach to check if it worked.
      With limit = 1_000_000_000 takes ~3min to run... ouch!
      We can lower the limit to 10_000_000, which takes only 8s.

   2. We can find the primes faster, if we first use the sieve technique to find primes factors
      (up to sqrt(limit)), and then use that prime table/list to check larger numbers for primality!
      Using this approach, with limit 1 bil we go down to ~1min, and to 3s with 10 mil.
      Not as fast as I would expect...

   3. The best approach would definitely be to generate all pandigital permutations
      and then check them for primality.
 */

let limit = 10_000_000;

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

/* we need to have a prime > sqrt(limit) */
let prime_list = list_of_primes 10_000;

let n = ref (limit-1);

while (not ((is_prime_with_sieve !n prime_list) && (is_pandigital !n))) {
  n := !n - 1;
};

print_int !n;
