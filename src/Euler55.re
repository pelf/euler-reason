/*
  Lychrel numbers
  Problem 55
  
  If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

  Not all numbers produce palindromes so quickly. For example,

  349 + 943 = 1292,
  1292 + 2921 = 4213
  4213 + 3124 = 7337

  That is, 349 took three iterations to arrive at a palindrome.

  Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
  A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
  Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume
  that a number is Lychrel until proven otherwise. In addition you are given that for every number
  below ten-thousand, it will either
  (i) become a palindrome in less than fifty iterations, or,
  (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome.
  In fact, 10677 is the first number to be shown to require over fifty iterations before producing a
  palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

  Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

  How many Lychrel numbers are there below ten-thousand?
*/

open Util;
open Big_int;

let limit = 10_000;

let is_lychrel n => {
  let bi_n = big_int_of_int n;
  let rec lyc bi iter => {
    switch iter {
      | 50 => true /* stop after 50 iterations */
      | _ => {
        /* reverse and add */
        let sum = bi_reverse bi |> add_big_int bi;
        /* get list of int digits */
        let int_digits = List.map (fun e => (int_of_big_int e)) (bi_digits sum);
        /* check if it's a palindrome */
        switch (is_palindrome int_digits) {
          | true => false /* it's not a lychrel */
          | false => lyc sum (iter+1)
        };
      };
    };
  };
  lyc bi_n 1;
};

let count = ref 0;

for i in 1 to limit {
  if (is_lychrel i) {
    count := !count + 1;
  }
};

string_of_int !count |> print_endline;
