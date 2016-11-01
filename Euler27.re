/*
  Quadratic primes
  Problem 27

  Euler discovered the remarkable quadratic formula:
  n^2 + n + 41
  It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39.
  However, when n=40,402+40+41=40(40+1)+41n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,412+41+41n=41,412+41+41 is clearly divisible by 41.

  The incredible formula n^2 − 79n + 1601 was discovered, which produces 80 primes for the consecutive
  values 0≤n≤79. The product of the coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

  n^2 + a*n + b , where |a|<1000 and |b|≤1000

  Find the product of the coefficients, a and b, for the quadratic expression that produces
  the maximum number of primes for consecutive values of n, starting with n=0.
*/

let is_prime n => {
  if (n <= 1) { false }
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

let quadratic n a b =>
  n*n + a*n + b;

let max_consecutive_primes a b => {
  let rec mcp a b n => {
    if (is_prime (quadratic n a b)) { mcp a b (n + 1) }
    else { n - 1 }
  };
  mcp a b 1;
};

let max = ref 0;

for a in (-999) to (999) {
  for b in (-1_000) to (1_000) {
    let mcp = max_consecutive_primes a b;
    if (mcp > !max) {
      max := mcp;
      print_endline ("a: " ^ (string_of_int a) ^ ", b: " ^ (string_of_int b) ^ ", mcp: " ^ (string_of_int mcp));
    }
  }
};
