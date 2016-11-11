/*
  Prime generating integers
  Problem 357

  Consider the divisors of 30: 1,2,3,5,6,10,15,30.
  It can be seen that for every divisor d of 30, d+30/d is prime.

  Find the sum of all positive integers n not exceeding 100 000 000
  such that for every divisor d of n, d+n/d is prime.
*/
open Utils;
open Big_int;

let limit = 100_000_000;

/* we need to have a prime > sqrt(limit) */
let primes = sieve (limit + 2);

let is_every_div_prime n => {
  /* only even numbers higher than 2 can do this */
  if ((n > 2) && (n mod 2 != 0)) {
    false
  } else {
    let sqrtn = int_of_float (sqrt (float_of_int n));
    let rec div n d => {
      if (d > sqrtn) { true }
      else {
        if ((n mod d == 0) && (not primes.(d + (n/d)))) {
          false
        } else {
          /* it's not a divisor or sum is prime, keep going! */
          div n (d+1);
        };
      };
    };
    div n 1;
  }
};

/* 1. first approach took 10 mins to run. LOL.

   2. skip odd numbers and numbers that are not followed by a prime (n+1)
      ~ 2 mins

   3. check divisors starting from 1 and going up instead of sqrtn and going down
      ~ 7 secs
*/

let sum = ref zero_big_int;

for n in 1 to limit {
  if (is_every_div_prime n) {
    sum := add_big_int !sum (big_int_of_int n);
  };
};

!sum |> string_of_big_int |> print_endline;
