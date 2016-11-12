/*
  Dice Game
  Problem 205

  Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
  Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

  Peter and Colin roll their dice and compare totals: the highest total wins.
  The result is a draw if the totals are equal.

  What is the probability that Pyramidal Pete beats Cubic Colin?
  Give your answer rounded to seven decimal places in the form 0.abcdefg
*/

open Utils;
open Big_int;

/* 1st approach: simulated games using random, but couldn't get enough precision in the result.

   2nd approach: calculate probability with permutations.

   (I'm pretty sure there's a mathematical way of calculating the final result directly, but hey!... :| )

   Notes: had to make a bunch of tail recursive versions for the usual list calls, to avoid stack overflows
*/

/* tail recursive List.map */
let tail_rec_map f l => {
  let rec map l acc => {
    switch l {
      | [] => acc
      | [hd,...tl] => map tl ([(f hd),...acc])
    };
  };
  map l [];
};

/* tail recursive List.flatten */
let tail_rec_flatten l => {
  let rec flatten l acc => {
    switch l {
      | [] => { acc }
      | [hd,...tl] => {
        let rec flat inner_l iacc => {
          switch inner_l {
            | [] => iacc;
            | [hd,...tl] => {
              flat (tl) ([hd,...iacc]);
            };
          };
        };
        let new_acc = flat hd acc;
        flatten tl new_acc;
      };
    };
  };
  flatten l [];
};

/* definitely not optimized.
   but I'm glad I figured out a simple clean way of generating these.
   (got slightly less clean once I made it tail recursive) */
let throws dice faces => {
  let rec throw dice perms => {
    /* each dice appends each possible face value to each one of the lists received */
    switch dice {
      | 0 => { perms }
      | _ => {
        /* perms is a list of lists */
        let res = tail_rec_map (fun l => { /* l is a list of ints */
          /* each call to append creates a new list appending the current n to the original list l.
             when all the calls finish, we have mapped each l to "faces" new lists */
          let rec append n acc => {
            switch n {
              | 0 => { acc }
              | _ => {
                append (n-1) [[n,...l],...acc]
              }
            };
          };
          append faces [];
        }) perms |> tail_rec_flatten; /* we need to flatten the result, because we just created a list of lists */
        throw (dice-1) res;
      };
    };
  };
  throw dice [[]];
};

let peter = tail_rec_map (list_sum) (throws 9 4);
let colin = tail_rec_map (list_sum) (throws 6 6);

let possible_cases = mult_big_int (big_int_of_int (power 4 9)) (big_int_of_int (power 6 6));

let pete_wins = {
  let wins = ref zero_big_int;
  List.iter (fun p => {
    List.iter (fun c => {
      if (p > c) {
        wins := succ_big_int !wins;
      }
    }) colin;
  }) peter;
  !wins;
};

/* we can't convert the big int to float or else we'll lose the precision, so I just multiply the divisor
   so we have all the digits we need as an int */
string_of_big_int (div_big_int (mult_big_int pete_wins (big_int_of_int 1_000_000_000)) possible_cases) |> print_endline;
