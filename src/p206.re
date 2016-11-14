/*
  Concealed Square
  Problem 206

  Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
  where each “_” is a single digit.
*/

open Big_int;
open Utils;

let format = [1,-1,2,-1,3,-1,4,-1,5,-1,6,-1,7,-1,8,-1,9,-1,0];

let check_format n => {
  let digs = bi_digits n;
  let rec check digs format => {
    switch digs {
      | [] => true
      | [hd,...tl] => {
        let [fhd,...ftl] = format;
        if (fhd == -1) { /* ignore this position */
          check tl ftl;
        } else if (fhd == hd){ /* digit matches */
          check tl ftl;
        } else { /* no cigar */
          false;
        }
      }
    }
  };
  check digs format;
};

/*
  This approach is a bit slow, it runs in around 1 min.
  Couldn't think of other ways to skip calculations.
*/

/* sqrts of 1020304050607080900 and 1929394959697989990 */
for n in 1_010_101_010 to 1_389_026_623 {
  /*
    1. square ends in 0, so we know the number has to end in zero,
       which also means the last digits are 00
    2. the following digit is a 9, so we know n has to end in 30 or 70
       (3 and 7 are the only nrs to produce a 9 when squared)
  */
  let m = n mod 100;
  if (m == 30 || m == 70) {
    let bi_n = big_int_of_int n;
    let sq = mult_big_int bi_n bi_n;
    if (check_format sq) {
      n |> string_of_int |> print_endline;
    }
  }
};
