/*
  Maximum path sum II
  Problem 67

  By starting at the top of the triangle below and moving to adjacent numbers on the row below,
  the maximum total from top to bottom is 23.

  3
  7 4
  2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'),
  a 15K text file containing a triangle with one-hundred rows.
*/

open Utils;
open Core.Std;
open Euler18Pyramid;

/* read text file */
let lines = read_file "input_data/67-triangle.txt";
/* triangle is now a list of lists of ints */
let triangle = List.map lines (fun e => { List.map (String.split e on::' ') int_of_string });

/* just re-use problem 18's code :) */
print_int (Pyramid.process_pyramid (List.rev triangle)); print_newline ();
