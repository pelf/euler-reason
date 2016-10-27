/**
 * Project Euler - Challenge 1
 */

let rec loop = fun i accum => {
  if (i == 0) { accum }
  else if ((i mod 3) == 0 || (i mod 5) == 0) { loop (i-1) (accum + i) }
  else { loop (i-1) accum }
};

print_int (loop 999 0);
