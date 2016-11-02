/*
  Project Euler - Problem 3

  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
*/

let number = 600851475143;
let factor = 2;

let rec fact = fun n f => {
  if (f*f <= number) {
    if ((n mod f) == 0) {
      print_int f;
      print_string ", ";
      fact (n/f) f;
    } else {
      fact n (f+1);
    }
  }
};

fact number factor;
