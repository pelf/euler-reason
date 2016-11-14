/*
  10001st prime
  Problem 7

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
  What is the 10 001st prime number?
*/

let is_prime n => {
  if (n == 1) { false }
  else if (n < 4 ) { true }
  else if ((n mod 2)==0) { false }
  else if ((n mod 3)==0) { false }
  else if (n < 9 ) { true }
  else {
    let sqrt_n = int_of_float(sqrt(float_of_int(n)));
    let rec prime_loop d => {
      if (d > sqrt_n) { true }
      else if ((n mod d) == 0) { false }
      else { prime_loop (d+2) }
    };
    prime_loop 5;
  };
};

let nth_prime n => {
  let rec nth_prime_aux n cur => {
    if (is_prime cur) {
      if (n == 1) { cur }
      else { nth_prime_aux (n-1) (cur+1) }
    } else {
      nth_prime_aux n (cur+1)
    }
  };
  nth_prime_aux n 1;
};

print_int (nth_prime 10_001)
