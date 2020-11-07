
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2);;
  
module Reader: sig
  val read_sexprs : string -> sexpr list
  (* val tok_bool_t : char list -> (char * char) * char list *)
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let nt_whitespaces_p = plus nt_whitespace;;
let nt_whitespaces_s = star nt_whitespace;;

let make_paired_char nt_left nt nt_right =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let make_spaced_char nt =
  make_paired_char nt_whitespaces_s nt nt_whitespaces_s;;

let make_paired_word nt_left nt nt_right = raise X_not_yet_implemented;;

let make_spaced_word nt = raise X_not_yet_implemented;;

(*
let tok_bool_t = 
  let _t = char_ci 't' in
  let _numbersign = char '#' in 
  let _tok_sign_t = pack (caten _numbersign _t) 
    (fun ((l, e), r) -> ((l::e), r)) in 
    caten make_spaced_char _tok_sign_t
*)

let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)
