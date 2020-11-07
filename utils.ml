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