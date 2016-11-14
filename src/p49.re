/*
  Prime permutations
  Problem 49

  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
  is unusual in two ways:
  (i) each of the three terms are prime, and,
  (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
  but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms in this sequence?
*/

open Utils;

let limit = 9_999;
let hash = Hashtbl.create limit;
let prime_list = bool_array_to_list (sieve limit);

/* stores each prime in the hashtable at the sorted digits position */
let rec store_primes l => {
  switch l {
    | [] => {}
    | [p, ...tl] => {
      if (p > 999) { /* ignore 3 digit nrs */
        let sorted_digs = List.sort compare (digits p);
        let canonical = digits_to_nr sorted_digs;
        Hashtbl.add hash canonical p;
      };
      store_primes tl;
    }
  }
};

/* check if the given list of (permutation) primes match the requirements */
let check_diffs l => {
  let arr = Array.of_list l;
  let length = Array.length arr;

  /* leave 2 elements out: one to calc the distance, and one to compare against */
  for i in 0 to (length-3) {
    let pi = arr.(i);
    for j in (i+1) to (length-2) {
      let pj = arr.(j);
      /* use difference between these two primes to check following primes */
      let diff = pi - pj;
      for k in (j+1) to (length-1) {
        let pk = arr.(k);
        if (diff == (pj - pk)) { /* diff match! */
          print_endline ("MATCH! p1:" ^ (string_of_int pi) ^ ", p2:"  ^ (string_of_int pj) ^ ", p3:" ^ (string_of_int pk) ^ ", diff:" ^ (string_of_int diff));
        }
      }
    }
  }
};

store_primes prime_list;

for i in 1_000 to limit {
  /* check if there's anything stored at this position */
  if (Hashtbl.mem hash i) {
    /* get all primes stored with this key */
    let primes = Hashtbl.find_all hash i;
    if ((List.length primes) > 2) {
      /* process them */
      check_diffs primes;
    };
  }
};
