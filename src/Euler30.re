/*
  Digit fifth powers
  Problem 30

  Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

  1634 = 1^4 + 6^4 + 3^4 + ^4^4
  8208 = 8^4 + 2^4 + 0^4 + 8^4
  9474 = 9^4 + 4^4 + 7^4 + 4^4
  As 1 = 1^4 is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
*/

open Util;

/* 9^5 * 7 < 1_000_000, so we can use 6 digit numbers as an upper bound */
let limit = 1_000_000;
let total = ref 0;

for i in 2 to limit {
  let digs = digits i;
  let dig_pow = List.map ( fun e => { power e 5 } ) digs;
  let sum = list_sum dig_pow;
  if (sum == i) {
    total := !total + i;
  }
};

print_int !total;
