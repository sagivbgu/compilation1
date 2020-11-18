#use "pc.ml";;
open PC;;

let make_paired nt_left nt nt_right=
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

(*
Line comments start with the semicolon character ; and continue
until either an end-of-line or end-of-input is reached.
The semicolon may appear anywhere on the line, and need not be the first character.
*)
  
let nt_semicolon = char ';';;

let nt_line_comment = 
  let nt_end_of_line = char '\n' in
  let nt_end_of_line = pack nt_end_of_line (fun _ -> []) in
  let nt_end_line_comment = disj nt_end_of_line nt_end_of_input in
  let nt_any = diff nt_any nt_end_line_comment in
  let nt_any_star = star nt_any in
  let nt = make_paired nt_semicolon nt_any_star nt_end_line_comment in
  pack nt (fun _ -> []);;

let nt_space =
  let nt_whitespace = pack nt_whitespace (fun _ -> []) in
  disj nt_whitespace nt_line_comment;;
let nt_space_star = star nt_space;;

let make_spaced nt =
  make_paired nt_space_star nt nt_space_star;;
