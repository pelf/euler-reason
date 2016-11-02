/*
  Special Pythagorean triplet
  Problem 9

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  a^2 + b^2 = c^2
  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
*/

let is_triplet a b c => {
  a*a + b*b == c*c;
};

let find_triplet_with_sum n => {
  /* a is at most 1/3 of n */
  for a in (1) to (n/3) {
    /* b is at most 1/2 of n */
    for b in (1) to (n/2) {
      /* they need to sum to n */
      let c = n - a - b;
      if (is_triplet a b c) {
        print_int (a*b*c);
      }
    }
  }
};

let sum = 1_000;

find_triplet_with_sum sum;
