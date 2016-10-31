/*
  Problem 18

  By starting at the top of the triangle below and moving to adjacent numbers on the row below,
  the maximum total from top to bottom is 23.
     3
    7 5
   2 4 6
  8 5 9 3
  That is, 3 + 7 + 4 + 9 = 23.
  Find the maximum total from top to bottom of the triangle below:
                              75
                            95  64
                          17  47  82
                        18  35  87  10
                      20  04  82  47  65
                    19  01  23  75  03  34
                  88  02  77  73  07  63  67
                99  65  04  28  06  16  70  92
              41  41  26  56  83  40  80  70  33
            41  48  72  33  47  32  37  16  94  29
          53  71  44  65  25  43  91  52  97  51  14
        70  11  33  28  77  73  17  78  39  68  17  57
      91  71  52  38  17  14  91  43  58  50  27  29  48
    63  66  04  68  89  53  67  30  73  16  69  87  40  31
  04  62  98  27  23  09  70  98  73  93  38  53  60  04  23
  NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
  However, Problem 67, is the same challenge with a triangle containing one-hundred rows,
  it cannot be solved by brute force, and requires a clever method! ,o)
*/

/* yay! \o/
   managed to solve this without aux or mutable structures
   and with an O(n) algo! */

let pyramid = [
  [75],
  [95, 64],
  [17, 47, 82],
  [18, 35, 87, 10],
  [20, 04, 82, 47, 65],
  [19, 01, 23, 75, 03, 34],
  [88, 02, 77, 73, 07, 63, 67],
  [99, 65, 04, 28, 06, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
];

/* debug method */
let print_list l => {
  let rec pl l => {
    switch l {
      | [] => ()
      | _ => { print_string (string_of_int(List.hd l)); print_string ", "; pl (List.tl l) }
    }
  };
  pl l; print_newline ();
};

/* maps l(i) to max(l(i),l(i+1)) */
let rec max_of_pairs l => {
  switch l {
    | [] => { assert false }
    | [hd] => { [hd] }
    | [hd, hd2, ...tl] => {
      let m = max hd hd2;
      [m, ...(max_of_pairs [hd2, ...tl])];
    }
  }
};

/* adds two lists together */
let rec add_lists l1 l2 => {
  switch (l1, l2) {
    /* they should be the same size, if not we just ignore the extra elements */
    | ([],_) | (_,[]) => { [] }
    | ([hd1,...tl1],[hd2,...tl2]) => { [(hd1+hd2), ...(add_lists tl1 tl2)] }
  };
};

/* this feels a little bit dirty. not sure why. the code could probably be cleaner... */
let process_pyramid pyr => {
  let rec process pyr prev_row => {
    switch pyr {
      | [] => { assert false } /* if the input is correct, we should never reach this */
      | [[hd]] => { hd + (List.hd prev_row) } /* sum the final element with the current max sum */
      | _ => {
        let cur_row = List.hd pyr;
        let sum = add_lists cur_row prev_row;
        process (List.tl pyr) (max_of_pairs sum);
      }
    };
  };
  let first = max_of_pairs (List.hd pyr);
  process (List.tl pyr) first;
};

print_int (process_pyramid (List.rev pyramid));
