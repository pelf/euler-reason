/*
  How many reversible numbers are there below one-billion?
  Problem 145

  Some positive integers n have the property that the sum [ n + reverse(n) ]
  consists entirely of odd (decimal) digits.
  For instance, 36 + 63 = 99 and 409 + 904 = 1313.
  We will call such numbers reversible; so 36, 63, 409, and 904 are reversible.
  Leading zeroes are not allowed in either n or reverse(n).

  There are 120 reversible numbers below one-thousand.

  How many reversible numbers are there below one-billion (109)?
*/

open Utils;

/* this approach was slower than I'd like (or expect) it to be :/ */

let is_reversible n => {
  let sum = n + (reverse n);
  List.for_all (fun d => d mod 2 == 1) (digits sum);
};

let count = ref 0;
let limit = 1_000_000_000;

for n in 1 to limit {
  if (n mod 10 != 0 && is_reversible n) {
    count := !count + 1;
  }
};

!count |> print_int;
