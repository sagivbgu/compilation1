#use "pc.ml";;
open PC;;

let nt_whitespaces_p = plus nt_whitespace;;
let nt_whitespaces_s = star nt_whitespace;;

let make_paired nt_left nt nt_right=
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let make_spaced nt =
  make_paired nt_whitespaces_s nt nt_whitespaces_s;;

(*
Line comments start with the semicolon character ; and continue
until either an end-of-line or end-of-input is reached.
The semicolon may appear anywhere on the line, and need not be the first character.
*)

(* I commented this out because it has errors that interfered with other files *)
(*
let nt_semicolon = char ';';;
let nt_end_line_comment = 
  let nt_end_of_line = char '\n' in
  disj nt_end_of_line nt_end_of_input;;
let nt_line_comment = 
  let nt = make_paired nt_semicolon nt_any nt_end_comment in
  pack nt (fun _ -> []);;
*)
let sexpr_comment_start = disj (char '#') nt_semicolon;;

(* TODO *)

(* catch the string parsed by nt followed by either End Of Input or 
  at least one whitespace 
  ==> char| or char( )+| *)
let make_nt_eoi_or_ws_on_the_right nt = 
  make_paired nt_epsilon nt (disj nt_end_of_input nt_whitespaces_p);;

(* catch the string parsed by whitespaces followed by nt followed by either 
  End Of Input or at least one whitespace 
  ==> ( )*char( )+| *)
let make_ws_star_nt_eoi_or_ws_plus nt = 
  make_paired nt_whitespaces_s nt (disj nt_end_of_input nt_whitespaces_p);;

let make_netto nt =
  make_spaced nt;;