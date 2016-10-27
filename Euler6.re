/*
  Project Euler - Problem 6

  The sum of the squares of the first ten natural numbers is,
  1^2 + 2^2 + ... + 10^2 = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^2 = 55^2 = 3025

  Hence the difference between the sum of the squares of the first ten natural numbers
  and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred natural numbers
  and the square of the sum.
*/

let square n =>
  n * n;

let sum l =>
  List.fold_left (+) 0 l;

let sum_of_squares l =>
  sum (List.map square l);

let square_of_sum = fun l =>
  square (sum l);

let range a b => {
  let rec rr a b l => {
    if (b < a) { l }
    /* how to append to a list? :: does not seem to work... */
    else { rr a (b-1) ([b] @ l) }
  };
  rr a b [];
};

let numbers = range 1 100;
print_int ((square_of_sum numbers)-(sum_of_squares numbers));
