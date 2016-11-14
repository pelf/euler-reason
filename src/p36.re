/*
  Double-base palindromes
  Problem 36

  The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

  Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
  (Please note that the palindromic number, in either base, may not include leading zeros.)
*/

/* returns a list with the digits of n in base b */
let digits n b => {
  let rec dig n b => {
    if (n==0) { [] }
    else { [(n mod b), ...(dig (n/b) b)] }
  };
  List.rev (dig n b);
};

/* checks if lists are equal to each other */
let rec equals l1 l2 => {
  switch (l1, l2) {
    | ([],_) | (_,[]) => { true }
    | ([hd1,...tl1],[hd2,...tl2]) => { (hd1 == hd2) && (equals tl1 tl2) }
  };
};

/* check if the given list is a palindrome */
let is_palindrome l => {
  /* checks if l equals reverse l */
  equals l (List.rev l);
};

let total = ref 0;

for i in (1) to (1_000_000) {
  if ( (is_palindrome (digits i 10)) && (is_palindrome (digits i 2)) ) {
    total := !total + i;
  }
};

print_int !total;
