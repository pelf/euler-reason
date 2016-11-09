open Core.Std;
open Big_int;
open Utils;

/* check if it's finding primes correctly */
let primes = sieve(100);
assert ((Array.length primes) == 100);
assert primes.(13);
assert (not primes.(14));

/* check if it converts array of primes to list correctly */
let prime_list = bool_array_to_list (sieve(10));
assert (List.length prime_list == 4);
assert ((List.hd prime_list) == Some 2);
assert ((List.hd (List.rev prime_list)) == Some 7);

/* check is_prime_with_sieve */
let prime_list = bool_array_to_list (sieve(2_000));
assert (is_prime_with_sieve 999377 prime_list);
assert (not (is_prime_with_sieve 999378 prime_list));

/* test range */
assert ((List.length (range 0 10)) == 11);

/* test explode */
assert ((List.length (explode "pelf") == 4));

/* test big_int hashtable */
let hash = BigIntHashtbl.create 100;
assert (not (BigIntHashtbl.mem hash Big_int.unit_big_int));
BigIntHashtbl.add hash Big_int.unit_big_int true;
assert (BigIntHashtbl.mem hash Big_int.unit_big_int);

/* test bi reverse */
assert (eq_big_int (bi_reverse (big_int_of_int 0)) (big_int_of_int 0));
assert (eq_big_int (bi_reverse (big_int_of_int 1)) (big_int_of_int 1));
assert (eq_big_int (bi_reverse (big_int_of_int 1234567)) (big_int_of_int 7654321));
assert (eq_big_int (bi_reverse (big_int_of_int 9321)) (big_int_of_int 1239));

/* test factorials */
let cache = Array.create 101 zero_big_int;
assert (eq_big_int (bi_factorial (big_int_of_int 100)) (big_int_of_int 3628800));
assert (eq_big_int (big_int_of_int 3628800) (factorial_with_cache 100 cache));
