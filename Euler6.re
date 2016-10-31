/**
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
	sum ( List.map square l);

let rec range = fun (a, b) => a < b ? [a, ...range( a + 1, b)] : [b];

let l100 = range (0, 100);
print_int ( square ( sum l100 ) - sum_of_squares l100 );

