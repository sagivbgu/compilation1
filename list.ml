#use "symbol.ml";;

(* 
  ⟨List⟩ ::= ( ⟨Sexpr⟩∗ )
  ⟨DottedList⟩ ::= ( ⟨Sexpr⟩+ . ⟨Sexpr⟩ )
 *)

let make_nt_parenthesized_expr nt =
  make_paired (make_spaced (char '(')) nt (make_spaced (char ')'));; 

(* Temporary, TODO: REMOVE!! *)
let nt_sexpr = 
  tok_number_ast;;

let nt_list = 
  let nt_sexpr = make_netto nt_sexpr in
  let nt_sexpr = star nt_sexpr in
  let parenth_sexpr = make_nt_parenthesized_expr nt_sexpr in
  parenth_sexpr;;

let nt_dotted_list = 
  let nt_sexpr = make_netto nt_sexpr in
  let p_sexpr = plus nt_sexpr in
  let _dot = make_netto dot in
  let dotted_sexpr = caten p_sexpr (caten _dot nt_sexpr) in
  let parenth_dotted_sexpr = make_nt_parenthesized_expr dotted_sexpr in
  let _remove_dot = 
    (function (l, (d, r)) -> l@[r]) in
  pack parenth_dotted_sexpr _remove_dot;;

let rec list_to_pairs lst = 
  match lst with
  | [] -> Nil
  | a::rest -> Pair (a,(list_to_pairs rest));;

(* TESTS *)

let nt_list_ast =
  pack nt_list list_to_pairs;;

let nt_dotted_list_ast = 
  pack nt_dotted_list list_to_pairs;;

nt_list (string_to_list "   (1 1    1)  ");;
nt_dotted_list (string_to_list " ( 1 2 3 . 4) ");;

Printf.printf " --- ";;

nt_list_ast (string_to_list "   (1 1    1)  ");;
nt_list_ast (string_to_list "   (   )  ");;

nt_dotted_list_ast (string_to_list " ( 1 2 3 . 4) ");;
nt_dotted_list_ast (string_to_list " ( 0.111 0.222 0.3333 . 0.4444) ");;
