/*
  Combinatoric selections
  Problem 53

  There are exactly ten ways of selecting three from five, 12345:

  123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

  In combinatorics, we use the notation, 5C3 = 10.

  In general,

  nCr =	n! / (r!(n−r)!) , where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.

  It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

  How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
*/

/*
  1. Try a naive approach first. Do not cache factorials, and calculate them on the fly.
     This was surprisingly fast. Runs in 0.15s. I guess the limit is too low to notice how
     bad this approach is...

  2. After caching the factorial calculations, it runs in 0.01s.

     Here are the running times with different limits:

        limit  |  approach 1  |  approach 2
         100   |    0.15 s    |    0.01s
         200   |    1.22 s    |    0.02s
         300   |    4.37 s    |    0.06s
         400   |   11.37 s    |    0.11s
         500   |   22.39 s    |    0.22s
*/

open Big_int;
open Utils;

let limit = 100;
let cache = Array.make (limit+1) zero_big_int;
/* cache all factorials we need */
factorial_with_cache limit cache;

let n_choose_r n r => {
  let n_fact = factorial_with_cache n cache;
  let r_fact = factorial_with_cache r cache;
  let n_r_fact = factorial_with_cache (n-r) cache;
  mult_big_int r_fact n_r_fact |> div_big_int n_fact;
};

let count = ref 0;

for n in 23 to limit {
  for r in 1 to limit {
    if (gt_big_int (n_choose_r n r) (big_int_of_int 1_000_000)) {
      count := !count + 1;
    }
  }
};

!count |> string_of_int |> print_endline;
