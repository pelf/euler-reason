open Core.Std;
open Big_int;
open Util;

let primes = sieve(100);
assert ((Array.length primes) == 100);
assert primes.(13);
assert (not primes.(14));

assert ((List.length (range 0 10)) == 11);

assert ((List.length (explode "pelf") == 4));

let hash = BigIntHashtbl.create 100;
assert (not (BigIntHashtbl.mem hash Big_int.unit_big_int));
BigIntHashtbl.add hash Big_int.unit_big_int true;
assert (BigIntHashtbl.mem hash Big_int.unit_big_int);
