/*

  Problem 15

  Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking)
  to the bottom right corner.

  #########   #####---.   #####---.
  |   |   #   |   #   |   |   #   |
  +---+---#   +---#####   +---#---+
  |   |   #   |   |   #   |   #   |
  '---+---#   '---+---#   '---#####

  #---+---.   #---+---.   #---+---.
  #   |   |   #   |   |   #   |   |
  #########   #####---+   #---+---+
  |   |   #   |   #   |   #   |   |
  '---+---#   '---#####   #########

  How many routes are there through a 20×20 grid?
*/

let n = 20;

/* Array to memoize nr of paths from each position, so we don't have to repeat calculations.
   Speeds things up A LOT. */
let grid = Array.make_matrix (n+1) (n+1) 0;
grid.(n).(n) = 1;
/* we could mark the right and bottom edges with 1 as well, but I'm lazy */

/* at each point we have as many paths as the sum of the paths from the 2 adjacent points (right & down) */
let count_paths n => {
  let rec cp n h v => {
    /* if we've never been here before, we need to do some more calls */
    if (grid.(h).(v) == 0) {
      /* if we're on the right edge, only go down */
      if (h==n) { grid.(h).(v) = (cp n h (v+1)) }
      /* if we're on the bottom edge, only go right */
      else if (v==n) { grid.(h).(v) = (cp n (h+1) (v)) }
      /* sum paths to the right and down */
      else { grid.(h).(v) = ((cp n (h+1) v) + (cp n h (v+1))) }
    };
    grid.(h).(v);
  };
  cp n 0 0;
};

print_int (count_paths n);
