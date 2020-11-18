#use "symbol.ml";;

(* 
  ⟨List⟩ ::= ( ⟨Sexpr⟩∗ )
  ⟨DottedList⟩ ::= ( ⟨Sexpr⟩+ . ⟨Sexpr⟩ )
 *)

let make_nt_parenthesized_expr nt =
  make_paired (char '(') nt (char ')');; 

let tok_list nt_sexpr = 
  let nt_sexpr = star nt_sexpr in
  let parenth_sexpr = make_nt_parenthesized_expr nt_sexpr in
  parenth_sexpr;;

let tok_dotted_list nt_sexpr = 
  let p_sexpr = plus nt_sexpr in
  let dotted_sexpr = caten p_sexpr (caten dot nt_sexpr) in
  let parenth_dotted_sexpr = make_nt_parenthesized_expr dotted_sexpr in
  let _remove_dot = 
    (function (l, (d, r)) -> l@[r]) in
  pack parenth_dotted_sexpr _remove_dot;;

let rec list_to_pairs lst = 
  match lst with
  | [] -> Nil
  | a::rest -> Pair (a,(list_to_pairs rest));;

let make nt_list nt_sexpr =
  pack (tok_list nt_sexpr) list_to_pairs;;

let make_nt_dotted_list nt_sexpr = 
  pack (tok_dotted_list nt_sexpr) list_to_pairs;;

(* TESTS *)

tok_list (string_to_list "   (1 1    1)  ");;
tok_dotted_list (string_to_list " ( 1 2 3 . 4) ");;

Printf.printf " --- ";;

nt_list (string_to_list "   (1 1    1)  ");;
nt_list (string_to_list "   (   )  ");;

nt_dotted_list (string_to_list " ( 1 2 3 . 4) ");;
nt_dotted_list (string_to_list " ( 0.111 0.222 0.3333 . 0.4444) ");;
