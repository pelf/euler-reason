/*
  Digit factorials
  Problem 34

  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

  Find the sum of all numbers which are equal to the sum of the factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*/

let rec factorial n => {
  switch n {
    | 0 | 1 => { 1 }
    | _ => { n * (factorial (pred n)) }
  }
};

/* returns a list of the digits of n */
let digits n => {
  let rec dig n => {
    if (n<10) { [n] }
    else { [(n mod 10), ...(dig (n/10))] }
  };
  List.rev (dig n);
};

let total = ref 0;

/* silly and super slow approach, I know.
   we could just keep multiplying the numbers together instead of calculating each factorial.
   only realized it after writing the factorial function, so I stuck with it out of laziness :P */
for i in (10) to (9_999_999) {
  let facts = List.map factorial (digits i);
  let sum_fact_of_digs = List.fold_left (+) 0 facts;
  if (sum_fact_of_digs == i) {
    total :=  !total + i;
  }
};

print_int !total;
