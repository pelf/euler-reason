/*
  Number spiral diagonals
  Problem 28

  Starting with the number 1 and moving to the right in a clockwise direction
  a 5 by 5 spiral is formed as follows:
    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13
  It can be verified that the sum of both diagonals is 101.
  What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same way?
*/

let n = 1_001;
let iters = n/2; /* technically (n-1)/2, but it's the same with int math */

/* start with the 1 in the middle */
let total = ref 1;
let prev = ref 1;

/* for each square, from the smaller to the largest */
for i in 1 to iters {
  /* step is the distance to the next diagonal value (corner) */
  let step = i * 2;
  /* there's 4 diagonal values (corners) in each square */
  for j in 1 to 4 {
    prev := !prev + step;
    total := !total + !prev;
  }
};

print_int !total;
print_newline ();
