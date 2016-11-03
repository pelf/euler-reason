/*
  Names scores
  Problem 22

  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing
  over five-thousand first names, begin by sorting it into alphabetical order.

  Then working out the alphabetical value for each name, multiply this value by its alphabetical
  position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order,
  COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
  So, COLIN would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?
*/

open Util;
open Core.Std;

/* read 1-line text file */
let line =
  switch (List.hd (read_file "input_data/22-names.txt")) {
    | Some l => { l }
    | None => { assert false }
  };

/* split and cleanup names */
let names = List.map (String.split line on::',') (fun n => String.sub n 1 ((String.length n)-2) );
/* sort them */
let sorted_names = List.sort compare names;

let name_value name => {
  let length = String.length name;
  let rec nv name i v => {
    switch (i == length) {
      | true => v
      | false => {
        let cval = (int_of_char (String.get name i))-64; /* A is 65 */
        nv name (i+1) (v+cval);
      }
    };
  };
  nv name 0 0;
};

let score i name =>
  (i+1) * (name_value name);

print_int (list_sum (List.mapi sorted_names score));
print_newline ();
