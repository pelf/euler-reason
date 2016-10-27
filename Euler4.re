/*
  Project Euler - Problem 4

  A palindromic number reads the same both ways. The largest palindrome made from the product
  of two 2-digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3-digit numbers.
*/

/* reverse given number without using strings. could be lazier :) */
let reverse n => {
  let rec rev n r => {
    if (n == 0) { r }
    /* pop last digit from n and append it to reversed nr */
    else { rev (n/10) (r*10 + (n mod 10)) }
  };
  rev n 0;
};

let is_pal n => {
  (n == (reverse n));
};

let largest = ref 0;

for a in (999) downto (100) {
  for b in (999) downto (a) {
    let prod = a * b;
    /* TODO break b loop if prod < largest */
    if ((is_pal prod) && (prod > !largest)) {
      largest := prod;
    };
  };
};

print_string (string_of_int (!largest));
