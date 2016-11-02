/**
 * Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed one million.
 */

let max = 1_000_000;

let rec sumNumbers = fun a b acc => {
  let c = a + b;
  if(c > max){
    acc;
  } else if(c mod 2 === 0){
    sumNumbers b c (acc + c);
  } else {
    sumNumbers b c acc;
  };
};


print_int (sumNumbers 1 1 0)
