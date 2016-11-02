/*
  Permuted multiples
  Problem 52

  It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits,
  but in a different order.

  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
*/

/* returns a list of the digits of n */
let nr_to_digits n => {
  let rec dig n => {
    if (n<10) { [n] }
    else { [(n mod 10), ...(dig (n/10))] }
  };
  List.rev (dig n);
};

/* returns a list with 2n 3n 4n 5n 6n */
let multiples n =>
  [2*n, 3*n, 4*n, 5*n, 6*n];

/* checks if lists are equal to each other */
let rec equals l1 l2 => {
  switch (l1, l2) {
    | ([],_) | (_,[]) => { true }
    | ([hd1,...tl1],[hd2,...tl2]) => { (hd1 == hd2) && (equals tl1 tl2) }
  };
};

/* n -> multiples -> map to digits -> sort -> all equal? */
for n in 1 to 1_000_000 {
  /* get multiples of n and convert each one to a sorted list of digits */
  let mults = List.map (fun e => (List.sort compare (nr_to_digits e))) (multiples n);
  /* do the same with the original nr */
  let nl = List.sort compare (nr_to_digits n);
  /* check if all multiples equal the original n */
  if (List.for_all (equals nl) mults) {
    print_endline (string_of_int n);
  }
};
