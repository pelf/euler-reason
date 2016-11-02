/*
  Power digit sum
  Problem 16

  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  What is the sum of the digits of the number 2^1000?
*/

open Big_int;

/* raise x to y-th power */
let rec power x y => {
  if (y == zero_big_int) { big_int_of_int 1 }
  else { mult_big_int x (power x (pred_big_int y)) };
};

let sum_digits n => {
  let rec sum n total =>
    if (n < (big_int_of_int 10)) { add_big_int total n }
    else { sum (div_big_int n (big_int_of_int 10)) (mod_big_int n (big_int_of_int 10)) };
  sum n zero_big_int;
};

print_string (string_of_big_int (sum_digits (power (big_int_of_int 2) (big_int_of_int 1_000))));
