/*
  Counting Sundays
  Problem 19

  You are given the following information, but you may prefer to do some research for yourself:
    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/

let is_leap year => {
  (year mod 4 == 0) && ( (not (year mod 100 == 0)) || (year mod 400 == 0) )
};

/* not very OCamly :/ should create a month type but I'm lazy */
let days_in_month year month => {
    switch month {
      | 0 | 2 | 4 | 6 | 7 | 9 | 11 => { 31 }
      | 3 | 5 | 8 | 10 => { 30 }
      | 1 => { (is_leap year) ? 29 : 28 }
      | _ => { assert false }
    }
};

let rec count_days year month dow => {
  if (year == 2001) { 0 }
  else {
    /* count this day if it's a sunday */
    let sunday = (dow == 6) ? 1 : 0;
    /* get ready for next month */
    let next_month = ((month + 1) mod 12);
    let next_year = ( (next_month == 0) ? (year+1) : (year) );
    let days = days_in_month year month;
    let next_dow = ((dow + days) mod 7);
    sunday + (count_days next_year next_month next_dow);
  };
};

/* Jan 1st 1901 was a tuesday */
print_int (count_days 1901 0 1);
