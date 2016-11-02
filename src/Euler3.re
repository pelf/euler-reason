/*
  Project Euler - Problem 3

  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
*/

/*
  let bigNumber = 600851475143;
*/

let bigNumber = 13195;

let isPrime n => {
	let rec prime = fun n c => c > 1 ? (n mod c !== 0 && prime n (c-1)) : true;
	prime n (n-1);
};


let rec factor = fun bN n acc => {
  (n === 1) ? [bN, ...acc] :
    bN mod n === 0 && isPrime n ?
      factor (bN / n) (bN / n - 1) [n, ...acc] :
      factor bN (n - 1) acc
  };

let list = factor bigNumber (bigNumber - 1) [];
print_string (String.concat " " (List.map string_of_int list))

