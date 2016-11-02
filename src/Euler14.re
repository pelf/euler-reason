/*
  Longest Collatz sequence
  Problem 14
  The following iterative sequence is defined for the set of positive integers:

  n → n/2 (n is even)
  n → 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:

  13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
  It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.
*/

let collatz n => {
  let rec col n c => {
    if (n==1) { c }
    else if ((n mod 2) == 0) { col (n/2) (c+1) }
    else { col (3 * n + 1) (c+1) }
  };
  col n 1;
};

let largest_seq = ref 0;
let starting_num = ref 0;

for i in (1) to (1_000_000) {
  if ((collatz i) > !largest_seq) {
    largest_seq := collatz i; /* yeah I know, doing it twice out of laziness */
    starting_num := i;
  }
};

print_int !starting_num;
