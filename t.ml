#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
let  unread_number n =
  match n with
  | Fraction(nom, denom) -> Printf.sprintf "%d/%d" nom denom
  | Float(f) -> Printf.sprintf "%f" f

let unread_char c =
  let scm_char_name = 
    match c with
    | '\n' -> "newline"
    | '\r' -> "return"
    | '\x00' -> "nul"
    | '\x0c' -> "page"
    | ' ' -> "space"
    | '\t' -> "tab"
    | _ -> String.make 1 c in
  Printf.sprintf "#\\%s" scm_char_name

let rec unread s = 
  match s with
  | Bool(true) -> Printf.sprintf "#t"
  | Bool(false) -> Printf.sprintf "#f"
  | Nil -> Printf.sprintf "()"
  | Number(n) -> unread_number n
  | Char(c) -> unread_char c
  | String(s) -> Printf.sprintf "\"%s\"" s
  | Symbol(s) -> Printf.sprintf "%s" s
  | Pair(car, cdr) -> Printf.sprintf "(%s . %s)" (unread car) (unread cdr);;

exception X_syntax_error;;
exception X_debug;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

let rec pairs_to_list = function
    | Nil -> []
    | Pair(first, second) -> first :: pairs_to_list second
;;

let rec tag_parse_expression = function
| Bool(b) -> Const(Sexpr(Bool(b)))
| Char(c) -> Const(Sexpr(Char(c)))
| Number(n) -> Const(Sexpr(Number(n)))
| String(s) -> Const(Sexpr(String(s)))
| Pair(Symbol("or"), exps) -> tag_parse_or (pairs_to_list exps)
| Pair(Symbol("set!"), exps) -> tag_parse_set (pairs_to_list exps)
| Pair(Symbol("begin"), exps) -> tag_parse_seq (pairs_to_list exps)
| Pair(Symbol("and"), exps) -> tag_parse_and exps
| Pair(Symbol("cond"), exps) -> tag_parse_cond exps
| Symbol(var) -> tag_parse_var var
| x -> Printf.printf "\n%s\n" (unread x); raise X_not_yet_implemented

and tag_parse_var var = 
  if (List.mem var reserved_word_list)
  then (raise X_syntax_error) 
  else (Var(var))

and tag_parse_or = function
  | [] -> Or([(tag_parse_expression (Bool(false)))])
  | lst -> Or((List.map tag_parse_expression lst))

and tag_parse_set = function
  | [exp1; exp2] -> Set ((tag_parse_expression exp1),(tag_parse_expression exp2))
  | x -> raise X_syntax_error

(* Seq needs to be flatten -> Seq([x ; Seq(y)]) => Seq([x; y]) *)
and tag_parse_seq = function
  | [] -> (Const Void)
  | [sexp] -> (tag_parse_expression sexp)
  | sexps -> Seq( (List.flatten (List.map flatten_seq (List.map tag_parse_expression sexps))) )

and flatten_seq = function
  | Seq(x) -> x
  | x -> [x]

(* External, used for "and", "cond" *)
and tag_parse_if = function
  | [if_test; if_then; if_else] -> If((tag_parse_expression if_test),
                                      (tag_parse_expression if_then),
                                      (tag_parse_expression if_else)
                                     )
  | [if_test; if_then] -> If((tag_parse_expression if_test),
                             (tag_parse_expression if_then),
                             Const(Void)
                            )
  | _ -> raise X_syntax_error

and tag_parse_and = function
  | Nil -> (tag_parse_expression (Bool(true)))
  | Pair(first, Nil) -> (tag_parse_expression first)
  | Pair(first, rest) -> (tag_parse_if [first ; Pair(Symbol("and"), rest) ; Bool(false)])

and tag_parse_cond = function
  | Pair(Pair(Symbol("else"), dit ), _) -> parse_cond_else_rib dit
  | Pair(Pair(cond, Pair(Symbol("=>"), func)), next) -> parse_cond_rib_2 cond func next
  | Pair(Pair(cond, dit), next) -> parse_cond_rib_1 cond dit next
  | x -> Printf.printf "\n%s\n" (unread x); raise X_debug

and parse_cond_rib_1 cond dit next = 
  If((tag_parse_expression cond), 
     (tag_parse_expression (Pair(Symbol("begin"), dit))), 
     (tag_parse_cond next))

and parse_cond_rib_2 cond func next =
  let expanded = 
    Pair (Symbol "let",
      Pair
      (Pair (Pair (Symbol "value", Pair (cond, Nil)),
        Pair
          (Pair (Symbol "f",
            Pair (Pair (Symbol "lambda", Pair (Nil, Pair (func, Nil))),
            Nil)),
          Pair
          (Pair (Symbol "rest",
            Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair(Symbol "cond", next), Nil))),
              Nil)),
          Nil))),
      Pair
        (Pair (Symbol "if",
          Pair (Symbol "value",
          Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
            Pair (Pair (Symbol "rest", Nil), Nil)))),
        Nil))) in
  (tag_parse_expression expanded)

and parse_cond_else_rib dit = 
  tag_parse_if [(Bool(true)); (Pair(Symbol("begin"), dit))]

;;

(* *************** TAG-PARSER ***************** *)
let tag_parse_expressions sexpr = List.map tag_parse_expression sexpr;;

end;; (* struct Tag_Parser *)

open Reader;;
open Tag_Parser;;