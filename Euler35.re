/*
  Circular primes
  Problem 35

  The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719,
  are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
*/

/* sieve code copied from prob 10. should move this to a reusable module */
/************  SIEVE OF ERATOSTHENES ****************/
let limit = 1_000_000;

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

let is_prime n => {
  if (n >= limit) { assert false }
  else { numbers.(n) }
};

sieve ();
/******************************************************************/

/* copied from prob 34 */
/* returns a list of the digits of n */
let nr_to_digits n => {
  let rec dig n => {
    if (n<10) { [n] }
    else { [(n mod 10), ...(dig (n/10))] }
  };
  List.rev (dig n);
};

/* raise x to y-th power */
let rec power x y => {
  if (y==0) { 1 }
  else { x * (power x (y-1)) }
};

/* convert digit list to nr */
let digits_to_nr l => {
  let rec to_n l exp => {
    switch l {
      | [] => { 0 }
      | [hd, ...tl] => { (hd * (power 10 exp)) + (to_n tl (exp + 1)) }
    }
  };
  to_n (List.rev l) 0;
};

/* return a list of all the rotations of the given list */
let rotations l => {
  let rec rot l times => {
    if (times == 0) { [] } /* this list will hold all the rotated lists */
    else {
      /* rotate list */
      let [hd, ...tl] = l;
      let rl = tl @ [hd];
      [l, ...(rot rl (times-1))];
    }
  };
  rot l (List.length l);
};

let count = ref 0;

for i in (0) to (limit-1) {
  /* check if all rotations are prime */
  let rots = List.map digits_to_nr (rotations (nr_to_digits i));
  if (List.for_all is_prime rots) {
    count := !count + 1;
  }
};

print_int !count;
print_newline ();
