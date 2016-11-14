/*
  Factorial digit sum
  Problem 20

  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!
*/

open Big_int;

let rec factorial n => {
  if (eq_big_int unit_big_int n) { unit_big_int }
  else { mult_big_int n (factorial (pred_big_int n)) }
};

let sum_digits n => {
  let ten = big_int_of_int 10;
  let rec sum n => {
    if (lt_big_int n ten) { n }
    else { add_big_int (mod_big_int n ten) (sum (div_big_int n ten)) }
  };
  sum n;
};

print_endline (string_of_big_int (sum_digits (factorial (big_int_of_int 100))));
