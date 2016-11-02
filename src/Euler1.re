/**
 * Euler 1 - Add all the natural numbers below 1000 that are multiples of 3 or 5.
 *
 */

let rec range = fun (a, b) => a < b ? [a, ...range(a + 1, b)] : [b];

let multiple_3_or_5 n => (n mod 3) === 0 || (n mod 5) === 0;

let sum = List.fold_left (+) 0 (List.filter multiple_3_or_5 (range (0, 1000)));

print_int sum

