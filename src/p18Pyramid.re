/* we're re-using this in prob 67 */

/* Yay! \o/
   Managed to solve this without aux or mutable structures and with an O(n) algo!
   Go go functional! */

let module Pyramid = {
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
};
