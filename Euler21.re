/*
  Amicable numbers
  Problem 21

  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called
  amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
  therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
*/

let sum_divisors n => {
  let rec sd n div => {
    if (div == 1) { 1 } /* 1 is always a divisor, so sum starts at 1 */
    else {
      /* if it's a divisor, we add it and its corresponding quotient */
      let sum = if (n mod div == 0) { div + (n / div) } else { 0 };
      sum + (sd n (div - 1));
    }
  };
  /* int_of_float truncates the number, effectively flooring it (floor is not working as expected...) */
  let sqrtn = int_of_float (sqrt (float_of_int n));
  sd n sqrtn;
};

let total = ref 0;

for a in (1) to (9_999) {
  let b = sum_divisors a;
  /* b > a means we avoid the case where b=a, and we don't count each pair twice (also a 2x speedup) */
  if ((b > a) && (a == (sum_divisors b))) {
      total := !total + a + b;
  }
};

print_int !total;
