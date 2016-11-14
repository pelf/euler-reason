/*
  Largest exponential
  Problem 99

  Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would
  confirm that 211 = 2048 < 37 = 2187.

  However, confirming that 632382518061 > 519432525806 would be much more difficult, as both numbers
  contain over three million digits.

  Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand
  lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.
*/
open Utils;
open Core.Std;

let raw_lines = read_file "input_data/99-base_exp.txt";
let values = List.map raw_lines (fun line => (List.map (String.split line on::',') int_of_string));

/*
  Numbers are way too big to compute normally, so I'll make use of the log properties:
    log(a^b) > log(c^d) => a^b > c^d, where: log(base^exp) = exp * log(base)
  This makes it super simple and fast to compute!
*/

let compute_exp pair => {
  switch pair {
    | [] | [_] => { failwith "invalid pair" }
    | [base, exp, ...tl] => {
        (log (float_of_int base)) *. (float_of_int exp);
    }
  }
};

let max = ref 0.0;

List.iteri values (fun i p => {
  let res = compute_exp p;
  if (res > !max) {
    max := res;
    /* print line nr to get the answer to the problem */
    (i+1) |> string_of_int |> print_endline;
  };
 });
