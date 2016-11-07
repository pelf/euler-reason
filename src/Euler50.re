/*
  Consecutive prime sum
  Problem 50

  The prime 41, can be written as the sum of six consecutive primes:

  41 = 2 + 3 + 5 + 7 + 11 + 13
  This is the longest sum of consecutive primes that adds to a prime below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime,
  contains 21 terms, and is equal to 953.

  Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/

open Util;

let limit = 1_000_000;

let prime_array = sieve limit;
let prime_list = bool_array_to_list prime_array;

let max = ref 0;

let rec check_chain list sum count => {
  switch list {
    | [] => {}
    | [hd,...tl] => {
      let cur_sum = sum + hd;
      if (cur_sum < limit) {
        if (prime_array.(cur_sum) && count > !max) {
          print_endline ((string_of_int cur_sum) ^ ", " ^ (string_of_int (count+1)));
          max := count;
        };
        check_chain tl cur_sum (count+1);
      } else {

      }
    };
  };
};

let rec find_chains list => {
  switch list {
    | [] => {}
    | [hd,...tl] => {
      check_chain list 0 0;
      find_chains tl;
    }
  };
};

find_chains prime_list;
