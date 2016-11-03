/* functions that are useful in more than one problem should be moved here! */

open Big_int;

/*******************************************
 prime helper functions
 */

/* sieve of erathostenes helper function.
   useful when we have an upper bound.
   returns array with primes marked with true */
let sieve limit => {
  /* array to mark numbers as not prime.
     0 and 1 are immediately marked with false because
     we will be crossing out multiples of each prime. */
  let primes = Array.make limit true;
  primes.(0) = false;
  primes.(1) = false;

  /* crosses out all multiples of i up to limit */
  let mark_multiples i => {
    let rec mm i e => {
      if (i*e < limit) {
        primes.(i*e) = false;
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
        if (primes.(i)) { mark_multiples(i) };
        rec_sieve (i+1);
      }
    };
    rec_sieve 2;
  };

  /* get to work! */
  sieve();
  /* return array with primes marked */
  primes;
};

/* when we don't have an upper bound and need to
   check a number individualy */
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

/********************************************
  list helper functions
  */

let rec range a b => {
  if (a > b) { [] }
  else { [a, ...(range (a+1) b)] }
};

/* print list of things. right print function needs to be passed in */
let print_list l f_print => {
  let rec pl l => {
    switch l {
      | [] => () /* done */
      | [hd, ...tl] => {
        f_print hd; /* uses given function to print whatever is in the list */
        print_string ", "; /* print separator */
        pl tl; /* keep going */
      }
    }
  };
  pl l; print_newline ();
};

/* multiply list of ints */
let list_mul l =>
  List.fold_left (*) 1 l;

/* sum list of ints */
let list_sum l =>
  List.fold_left (+) 0 l;

/* adds two lists of ints together */
let rec add_lists l1 l2 => {
  switch (l1, l2) {
    /* they should be the same size, if not we just ignore the extra elements */
    | ([],_) | (_,[]) => { [] }
    | ([hd1,...tl1],[hd2,...tl2]) => { [(hd1+hd2), ...(add_lists tl1 tl2)] }
  };
};

/* checks if lists are equal to each other */
let rec equals l1 l2 => {
  switch (l1, l2) {
    | ([],_) | (_,[]) => { true }
    | ([hd1,...tl1],[hd2,...tl2]) => { (hd1 == hd2) && (equals tl1 tl2) }
  };
};

/* returns a list of the digits of n */
let digits n => {
  let rec dig n => {
    if (n<10) { [n] }
    else { [(n mod 10), ...(dig (n/10))] }
  };
  List.rev (dig n);
};

/********************************************
  int helper functions
  */

/* raise x to y-th power */
let rec power x y => {
  if (y==0) { 1 }
  else { x * (power x (y-1)) }
};

let square n =>
  n * n;

/* sum digits of n */
let sum_digits n => {
  let ten = big_int_of_int 10;
  let rec sum n => {
    if (lt_big_int n ten) { n }
    else { add_big_int (mod_big_int n ten) (sum (div_big_int n ten)) }
  };
  sum n;
};

let rec factorial n => {
  switch n {
    | 0 | 1 => { 1 }
    | _ => { n * (factorial (pred n)) }
  }
};

/********************************************
  divisor helper functions
  */

let count_divisors n => {
  let rec cd n div count => {
    if (div <= 1) { count }
    else if ((n mod div) == 0) { cd n (pred div) (count + 2) }
    else { cd n (pred div) count }
  };
  /* int_of_float truncates the number, effectively flooring it (floor is not working as expected...) */
  let sqrtn = int_of_float (sqrt (float_of_int n));
  /* perfect squares have an odd number of divisors, so we subtract 1 to adjust the count */
  if (sqrtn*sqrtn == n) { cd n sqrtn 1 }
  else { cd n sqrtn 2 }
};

let sum_divisors n => {
  let rec sd n div => {
    if (div == 1) { 1 } /* 1 is always a divisor, so sum starts at 1 */
    else {
      /* if it's a divisor, we add it and its corresponding quotient */
      let sum = if (n mod div == 0) { div + (n / div) } else { 0 };
      sum + (sd n (div - 1));
    }
  };
  /* int_of_float truncates the number, effectively flooring it (floor is not working as expected...) */
  let sqrtn = int_of_float (sqrt (float_of_int n));
  sd n sqrtn;
};


/********************************************
  big int helper functions
  */

let rec bi_factorial n => {
  if (eq_big_int unit_big_int n) { unit_big_int }
  else { mult_big_int n (bi_factorial (pred_big_int n)) }
};

let bi_sum_digits n => {
  let ten = big_int_of_int 10;
  let rec sum n => {
    if (lt_big_int n ten) { n }
    else { add_big_int (mod_big_int n ten) (sum (div_big_int n ten)) }
  };
  sum n;
};

/* raise x to y-th power */
let rec bi_power x y => {
  if (eq_big_int y zero_big_int) { big_int_of_int 1 }
  else { mult_big_int x (bi_power x (pred_big_int y)) };
};


/*******************************************
  string helper functions
  */
let explode s => {
  let length = String.length s;
  let rec exp i => {
    if (i == length) { [] }
    else { [ (String.sub s i 1), ...(exp (i+1)) ] }
  };
  exp 0;
};
