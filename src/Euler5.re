/*
  Project Euler - Problem 5

  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/

/* we should make this dynamic if N is a larger number */
let primes = [2, 3, 5, 7, 11, 13, 17, 19];

/* raise x to y-th power */
let rec power x y => {
  if (y==0) { 1 }
  else { x * (power x (y-1)) }
};

/* what's the largest power (k) of p that goes into n?
   we could use floor(log()) but god knows how you do that in reason */
/* this should be "find_k p n", instead of having n hard coded, but couldn't
   figure out how to then call it within a List.map with a fixed parameter :( */
let find_k p => {
  let rec fk p n k => {
    if ((power p k) > n) { k - 1 }
    else { fk p n (k + 1) }
  };
  fk p 20 1;
};

/* returns p raised to the result of find_k(p) */
let k_power p => {
  power p (find_k p);
};

/* multiply values in the list */
let prod l =>
  List.fold_left (*) 1 l;

/* multiply the list of primes raised to the highest power that goes into N,
   or in ruby lingo: map{prime -> prime^(K)}.reduce(:*)
*/
print_int (prod (List.map (k_power) primes));
