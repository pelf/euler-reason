/*
  Spiral primes
  Problem 58

  Starting with 1 and spiralling anticlockwise in the following way,
  a square spiral with side length 7 is formed.

  37 36 35 34 33 32 31
  38 17 16 15 14 13 30
  39 18  5  4  3 12 29
  40 19  6  1  2 11 28
  41 20  7  8  9 10 27
  42 21 22 23 24 25 26
  43 44 45 46 47 48 49

  It is interesting to note that the odd squares lie along the bottom right diagonal,
  but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime;
  that is, a ratio of 8/13 ≈ 62%.

  If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
  If this process is continued, what is the side length of the square spiral for which the ratio of primes along
  both diagonals first falls below 10%?
*/

open Utils;

let primes = list_of_primes 100_000;

/* start with the 1 in the middle */
let prev = ref 1;
let over = ref false;
let i = ref 0;
let prime_count = ref 0;

while(not !over) {
  i := !i + 1;
  /* step is the distance to the next diagonal value (corner) */
  let step = !i * 2;
  /* number of items in the diagonals: 4 per layer + center */
  let diag_count = 1 + 4 * !i;
  /* there's 4 diagonal values (corners) in each square */
  for j in 1 to 4 {
    prev := !prev + step;
    if (is_prime_with_sieve !prev primes) {
      prime_count := !prime_count + 1;
    }
  };
  let perc = (float_of_int !prime_count) /. (float_of_int diag_count);
  if (perc < 0.1) { over := true }
};

/* last side length: */
print_int (!i * 2 + 1);
