(* hex-arithmetic-parser.ml
 * A parser for a simple language that consists of
 * - hex integers of the form 0x1234ABCdef
 * - The operators +, *, ^ with increasing precedence
 *
 * Programmer: Mayer Goldberg, 2019
 *)

#use "pc.ml";;

open PC;;

let nt_whitespace = const (fun ch -> ch <= ' ');;

let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let make_spaced nt =
  make_paired (star nt_whitespace) (star nt_whitespace) nt;;

let nt_hex =
  let make_nt_digit ch_from ch_to displacement =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - displacement in
		      fun ch -> (Char.code ch) - delta) in
    nt in
  let nt = disj (make_nt_digit '0' '9' 0)
		(make_nt_digit 'a' 'f' 10) in
  let nt = disj nt (make_nt_digit 'A' 'F' 10) in
  let nt = plus nt in
  let nt = pack nt (fun digits ->
		    List.fold_left (fun a b -> 16 * a + b) 0 digits) in
  let nt = caten (word_ci "0x") nt in
  let nt = pack nt (function (_0x, e) -> e) in
  make_spaced nt;;

type expr =
  | Number of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Power of expr * expr;;

let make_nt_parenthesized_expr nt =
  let nt1 = make_paired (make_spaced (char '(')) 
			(make_spaced (char ')')) nt in
  let nt2 = make_paired (make_spaced (char '[')) 
			(make_spaced (char ']')) nt in
  let nt3 = make_paired (make_spaced (char '{'))
			(make_spaced (char '}')) nt in
  let nt = disj nt1 (disj nt2 nt3) in
  nt;;

let nt_expr = 
  let rec nt_parenthesized_expr s =
    make_nt_parenthesized_expr nt_L0 s
  and nt_L3 s =
    disj nt_parenthesized_expr
	 (pack nt_hex (fun e -> Number e))
	 s 
  and nt_L2 s =
    let nt = caten (make_spaced (char '^'))
		   nt_L3 in
    let nt = pack nt (function (_, e) -> e) in
    let nt = star nt in
    let nt = caten nt_L3 nt in
    let nt = pack nt (fun (e1, es) ->
		      List.fold_left (fun a b -> Power(a, b))
				     e1
				     es) in
    nt s
  and nt_L1 s =
    let nt = caten (make_spaced (char '*'))
		   nt_L2 in
    let nt = pack nt (function (_, e) -> e) in
    let nt = star nt in
    let nt = caten nt_L2 nt in
    let nt = pack nt (function (e1, es) ->
			       List.fold_left (fun a b -> Mult(a, b))
					      e1
					      es) in
    nt s
  and nt_L0 s =
    let nt = caten (make_spaced (char '+'))
		   nt_L1 in
    let nt = pack nt (function (_, e) -> e) in
    let nt = star nt in
    let nt = caten nt_L1 nt in
    let nt = pack nt (function (e1, es) ->
			       List.fold_left (fun a b -> Plus(a, b))
					      e1
					      es) in
    nt s in
  nt_L0;;
  
  (* HOW TO USE THIS CODE:

# test_string nt_expr "   0x2 * 0x3 + 0x4 * 0x5 ";;
- : expr * string =
(Plus (Mult (Number 2, Number 3), Mult (Number 4, Number 5)), "->[]")

# test_string nt_expr "  0xabc + 0xABC * 0xAbC ^ 0xabC";;
- : expr * string =
(Plus (Number 2748, Mult (Number 2748, Power (Number 2748, Number 2748))),
 "->[]")

   *)
