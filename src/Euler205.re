/*
  Dice Game
  Problem 205

  Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
  Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

  Peter and Colin roll their dice and compare totals: the highest total wins. The result is a draw if the totals are equal.

  What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to seven decimal places in the form 0.abcdefg
*/

open Random;

Random.self_init ();

let throw throws sides => {
  let rec throw_aux n s => {
    switch n {
      | 0 => { 0 }
      | _ => {
        ((Random.int s) + 1) + (throw_aux (n-1) s);
      }
    };
  };
  throw_aux throws sides;
};

let wins = ref 0;
let games = ref 0;

for i in 1 to 100_000_000 {
  games := !games + 1;
  if ((throw 9 4) > (throw 6 6)) {
    wins := !wins + 1;
  };
};

(float_of_int !wins) /. (float_of_int !games) |> string_of_float |> print_endline;
