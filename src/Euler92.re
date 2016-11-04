/*
  Square digit chains
  Problem 92

  A number chain is created by continuously adding the square of the digits in a number
  to form a new number until it has been seen before.

  For example,

  44 → 32 → 13 → 10 → 1 → 1
  85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

  Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
  What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

  How many starting numbers below ten million will arrive at 89?
*/

open Util;

/*
  1. On my first approach I simply brute forced it, which meant it took around 3.2 seconds to run.
  It could easily be optimized by memoizing the result for each number in case we saw them again.

  /* first approach. looked way cleaner, but it was slower. */
  let rec chain n => {
    switch n {
      | 1 | 89 => { n }
      | _ => { chain (square_of_digits n) }
    }
  };

  2. Memoization resulted in a 3x speedup. The new version takes around 1s to finish.

  It could be way faster if I just did it arithmetically instead of generating lists of digits,
  but I'm lazy and just re-used functions I'd written before in Util.

  3. Modified square_of_digits to do it arithmetically instead of using lists.
     It now runs in 0.3s.

     It could probably be optimized way more, but I can't be a**ed to figure it out...
*/

/* biggest nr we can reach is when n=9_999_999 => 9^2 * 7 = 567 */
let cache = Array.make 600 (None);

let rec square_of_digits n => {
  if (n < 10) { n*n }
  else {
    let d = (n mod 10);
    (d*d) + (square_of_digits (n/10));
  }
};

let chain n => {
  let rec ch n => {
    switch n {
      /* we completed the chain for the given n */
      | 1 | 89 => { n }
      | _ => {
        switch (cache.(n)) {
          /* we've calculated this chain before */
          | Some r => { r }
          /* keep going */
          | None => { ch (square_of_digits n) }
        }
      }
    }
  };
  /* we calculate the first value outside of the loop so we can simply memoize nrs under 567 */
  let first = square_of_digits n;
  let res = ch first;
  /* memoize this n */
  cache.(first) = Some res;
  res;
};

let count = ref 0;

for n in 1 to 10_000_000 {
  if ((chain n) == 89) {
    count := !count + 1;
  }
};

print_int !count;
