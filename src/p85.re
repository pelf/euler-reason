/*
  Counting rectangles
  Problem 85

  By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles.

  Although there exists no rectangular grid that contains exactly two million rectangles,
  find the area of the grid with the nearest solution.
*/

/* `count_rectangles` could definitely be done analytically, but it runs pretty fast anyway... */

let count_rectangles gw gh => {
  let count = ref 0;
  for pw in 1 to gw {
    for ph in 1 to gh {
      let n = (gw-pw+1)*(gh-ph+1);
      count := !count + n;
    }
  };
  !count;
};

let target = 2_000_000;
let closest = ref 0;
let dist = ref 1_000;

for gw in 1 to 100 {
  for gh in 1 to 100 {
    let c = count_rectangles gw gh;
    let d = abs (target - c);
    if (d < !dist) {
      print_endline ((string_of_int gw) ^ " " ^ (string_of_int gh) ^ " " ^ (string_of_int c));
      dist := d;
      closest := c;
    }
  }
}
