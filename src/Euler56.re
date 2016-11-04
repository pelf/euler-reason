/*
  Powerful digit sum
  Problem 56

  A googol (10^100) is a massive number: one followed by one-hundred zeros;
  100^100 is almost unimaginably large: one followed by two-hundred zeros.
  Despite their size, the sum of the digits in each number is only 1.

  Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
*/
open Big_int;
open Util;

let limit = 99;
let max = ref 0;

for a in 2 to limit {
  for b in 2 to limit {
    let pow = bi_power (big_int_of_int a) (big_int_of_int b);
    let sum = int_of_big_int (bi_sum_digits pow);
    if (sum > !max) {
      max := sum;
    }
  };
};

print_int !max;
print_newline ();
