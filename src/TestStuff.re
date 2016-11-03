open Core.Std;
open Big_int;
open Util;

print_endline (string_of_big_int (big_int_of_int 1));

let primes = sieve(100);
for i in 0 to 99 {
  if (primes.(i)) {
    print_int i; print_newline ();
  }
};

print_list (range 0 10) print_int;

print_list (explode "pelf") print_string;
