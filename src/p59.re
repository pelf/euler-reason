/*
  XOR decryption
  Problem 59

  Each character on a computer is assigned a unique code and the preferred standard is ASCII.
  For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

  A modern encryption method is to take a text file, convert the bytes to ASCII,
  then XOR each byte with a given value, taken from a secret key.
  The advantage with the XOR function is that using the same encryption key on the cipher text,
  restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

  For unbreakable encryption, the key is the same length as the plain text message, and the key is
  made up of random bytes. The user would keep the encrypted message and the encryption key in
  different locations, and without both "halves", it is impossible to decrypt the message.

  Unfortunately, this method is impractical for most users, so the modified method is to use a password
  as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically
  throughout the message. The balance for this method is using a sufficiently long password key for security,
  but short enough to be memorable.

  Your task has been made easy, as the encryption key consists of three lower case characters.
  Using cipher.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes,
  and the knowledge that the plain text must contain common English words, decrypt the message and find
  the sum of the ASCII values in the original text.
*/

open Utils;
open Core.Std;

let line = read_line_from_file "input_data/59-cipher.txt";
let values = List.map (String.split line on::',') int_of_string;

/* list int -> list int -> list int */
let decode cipher k => {
  let key = Array.of_list k;
  List.mapi cipher (fun i c => { c lxor (key.(i mod 3)) });
};

let count_valid_chars l => {
  let valid = List.map l (fun c => { (c >= 97 && c <= 122) ? 1 : 0 });
  list_sum valid;
};

let m = ref 0;

for k1 in 97 to 122 {
  for k2 in 97 to 122 {
    for k3 in 97 to 122 {
      let key = [k1, k2, k3];
      let decoded = decode values key;
      let valid = count_valid_chars decoded;
      if (valid > !m) {
        m := valid;
        print_endline "\n";
        print_list key print_int;
        list_sum decoded |> string_of_int |> print_endline;
        List.iter decoded (fun c => { print_char (char_of_int c) });
      };
    };
  };
};
