/* functions that are useful in more than one problem should be moved here! */

open Big_int;

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
   check a smallish number individualy */
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

/* converts prime array into list of primes */
let bool_array_to_list arr => {
  let limit = Array.length arr;
  let rec atol arr i => {
    switch (i==limit) {
      | true  => []
      | false => switch arr.(i) {
          | true => [i, ...(atol arr (i+1))] /* i is prime, append it to array */
          | false => atol arr (i+1) /* i is not prime, keep going */
      }
    }
  };
  atol arr 0;
};

/* get list of primes up to n */
let list_of_primes n => {
  bool_array_to_list (sieve n)
};

/* we can combine the sieve and the loop together when dealing with really large bounded values.
   we use the sieve to find prime factors to then check larger numbers with them.
   WARNING: make sure the sieve finds a prime LARGER than sqrt(n) */
let is_prime_with_sieve n prime_list => {
  if (n <= 1) { false }
  else {
    let sqrt_n = int_of_float(sqrt(float_of_int(n)));
    let rec prime_loop l => {
      switch l {
        | [] => { assert false }
        | [d,...tl] => {
          if (d > sqrt_n) { true }
          else if ((n mod d) == 0) { false }
          else { prime_loop tl }
        };
      };
    };
    prime_loop prime_list;
  };
};

/********************************************
  int helper functions
  */

/* reverses given number: 923 -> 329 */
let reverse n => {
  let rec rev n rev_n => {
    if (n == 0) { rev_n }
    else {
      /* multiply rev_n by 10 (shift it left) and add current digit */
      rev_n * 10 + (n mod 10) |> rev (n / 10);
    }
  };
  rev n 0;
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

/* check if the given list is a palindrome */
let is_palindrome l => {
  /* checks if l equals reverse l */
  equals l (List.rev l);
};

/* returns a list of the digits of n */
let digits n => {
  let rec dig n => {
    if (n<10) { [n] }
    else { [(n mod 10), ...(dig (n/10))] }
  };
  List.rev (dig n);
};

/* convert digit list back to nr */
let digits_to_nr l => {
  let rec to_n l exp => {
    switch l {
      | [] => { 0 }
      | [hd, ...tl] => { (hd * (power 10 exp)) + (to_n tl (exp + 1)) }
    }
  };
  to_n (List.rev l) 0;
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

/*
  Make use of the "functorial interface" to create a Big_int hashtable.
  Check relevant section here: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html
*/
let module BigIntHash = {
  type t = big_int;
  let equal = eq_big_int;
  let hash n => { Hashtbl.hash (int_of_big_int (mod_big_int n (big_int_of_int max_int))) };
};
let module BigIntHashtbl = Hashtbl.Make(BigIntHash);

/* big_int -> big_int */
let rec bi_factorial n => {
  if (le_big_int n unit_big_int) { unit_big_int }
  else { mult_big_int n (bi_factorial (pred_big_int n)) }
};

/* int -> array int -> big_int */
let factorial_with_cache n cache => {
  let rec fact n => {
    if (n <= 1) { unit_big_int }
    else {
      /* do we have this result cached? */
      if (not (eq_big_int cache.(n) zero_big_int)) {
        cache.(n);
      } else {
        let f = mult_big_int (big_int_of_int n) (fact (n-1));
        cache.(n) = f; /* cache it! */
        f; /* return it */
      };
    };
  };
  fact n;
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
let bi_power x y => {
  let rec pow x y res => {
    if (eq_big_int y zero_big_int) { res }
    else { pow x (pred_big_int y) (mult_big_int x res) };
  };
  pow x y unit_big_int;
};

/* returns a list of the digits of big_int n */
let bi_digits n => {
  let ten = big_int_of_int 10;
  let rec dig n => {
    if (lt_big_int n ten) { [(int_of_big_int n)] }
    else { [(int_of_big_int (mod_big_int n ten)), ...(dig (div_big_int n ten))] }
  };
  List.rev (dig n);
};

/* reverses given number: 923 -> 329 */
let bi_reverse n => {
  let ten = big_int_of_int 10;
  let rec rev n rev_n => {
    if (eq_big_int n zero_big_int) { rev_n }
    else {
      /* multiply rev_n by 10 (shift it left) and add current digit */
      mult_big_int rev_n ten |> add_big_int (mod_big_int n ten) |> rev (div_big_int n ten);
    }
  };
  rev n zero_big_int;
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


/*******************************************
  file helper functions
  */

let read_file filename => {
  let lines = ref [];
  let chan = open_in filename;
  try {
    while true {
      lines := [(input_line chan), ... !lines];
    };
    !lines;
  } { /* this is SUCH a weird syntax! took me a while to figure it out :| */
    | End_of_file => {
      close_in chan;
      List.rev !lines;
    };
  };
};

/* read 1-line text file */
let read_line_from_file filename => {
  List.hd (read_file filename)
};
