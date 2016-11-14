/*
  Non-abundant sums
  Problem 23

  A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
  For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
  which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than n
  and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
  the smallest number that can be written as the sum of two abundant numbers is 24.

  By mathematical analysis, it can be shown that all integers greater than 28123 can be written
  as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis
  even though it is known that the greatest number that cannot be expressed as the sum of two abundant
  numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*/

/* adapted from prob 21 */
let sum_divisors n => {
  let rec sd n div => {
    if (div == 1) { 1 } /* 1 is always a divisor, so sum starts at 1 */
    else {
      /* if it's a divisor, we add it and its corresponding quotient */
      let sum = if (n mod div == 0) { div + (n / div) } else { 0 };
      sum + (sd n (div - 1));
    }
  };
  let sqrtn = int_of_float (sqrt (float_of_int n));
  sd n sqrtn;
};

let is_abundant n =>
  (sum_divisors n) > n;

let rec calc_abundant n => {
  if (n==12) { [12] }
  else if (is_abundant n) { [n, ...(calc_abundant (n-1))] }
  else { calc_abundant (n-1) }
};

let limit = 28_123;

/* list of all abundant nrs under the limit */
let abundant = Array.of_list (calc_abundant limit);
/* array to mark the numbers that we can make as a sum */
let is_sum = Array.make limit false;

/* lazy and tired of recursion... :'( */
for i in 0 to ((Array.length abundant)-1) {
  let a1 = abundant.(i);
  for j in i to ((Array.length abundant)-1) {
    let a2 = abundant.(j);
    let sum = a1 + a2;
    if (sum < limit) {
      is_sum.(sum) = true;
    };
  };
};

let total = ref 0;

for i in 0 to (limit-1) {
  if (not is_sum.(i)) {
    total := !total + i;
  }
};

print_int (sum_divisors 28);
