/*
  Summation of primes
  Problem 10

  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  Find the sum of all the primes below two million.
*/

let limit = 2_000_000;

/* array to mark numbers as not prime.
   0 and 1 are immediately marked with false because
   we will be crossing out multiples of each prime. */
let numbers = Array.make limit true;
numbers.(0) = false;
numbers.(1) = false;

/* crosses out all multiples of i up to limit */
let mark_multiples i => {
  let rec mm i e => {
    if (i*e < limit) {
      numbers.(i*e) = false;
      mm i (e+1);
    }
  };
  /* we could start at i*2, but we don't need to start before i^2,
     because all multiples below that have already been crossed out before */
  mm i i;
};

/* every number we reach not marked with false is a prime.
   we then cross out all multiples of that number */
let sieve => {
  let rec rec_sieve i => {
    if (i < (limit-1)) {
      if (numbers.(i)) { mark_multiples(i) };
      rec_sieve (i+1);
    }
  };
  rec_sieve 2;
};

/* sum all the numbers marked as prime.
   should be called after a call to `sieve ()` */
let sum_primes => {
  let rec sum_rec i sum => {
    if (i < (limit-1)) {
      if (numbers.(i)) { sum_rec (i+1) (sum + i) }
      else { sum_rec (i+1) sum }
    } else { sum }
  };
  sum_rec 0 0;
};

sieve ();
print_int (sum_primes ());
print_newline ();
