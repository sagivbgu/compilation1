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


exception X_syntax_error;;

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

  (* *************** UTILS ***************** *)
  type 'a pairlist = 
    | ProperList of 'a list
    | ImproperList of 'a list * 'a;; (* A tuple of the form: (list_except_last_item, last_item) *)

  let rec pair_to_pairlist = function
    | Nil -> ProperList([])
    | Pair(car, cdr) -> (
        let cdr_pairlist = pair_to_pairlist cdr
        in let append_to_pairlist car cdr_pairlist =
             match cdr_pairlist with
             | ProperList(lst) -> ProperList(car :: lst)
             | ImproperList(lst, last) -> ImproperList(car :: lst, last)
        in append_to_pairlist car cdr_pairlist)
    | last -> ImproperList([], last)

  (* *************** EXPR ***************** *)
  let rec tag_parse_expression = function
    | Bool(b) -> Const(Sexpr(Bool(b)))
    | Char(c) -> Const(Sexpr(Char(c)))
    | Number(n) -> Const(Sexpr(Number(n)))
    | String(s) -> Const(Sexpr(String(s)))
    | Pair (Symbol "quote", Pair(sexpr, Nil)) -> Const(Sexpr(sexpr)) (* Intentionally not recursive *)
    | Pair (Symbol "if", rest_of_if) ->  tag_parse_if (pair_to_pairlist rest_of_if)
    | Pair (Symbol "lambda", rest_of_lambda) ->  tag_parse_lambda (pair_to_pairlist rest_of_lambda)

  and tag_parse_if = function
    | ProperList([if_test; if_then; if_else]) -> If((tag_parse_expression if_test),
                                                    (tag_parse_expression if_then),
                                                    (tag_parse_expression if_else)
                                                   )
    | ProperList([if_test; if_then]) -> If((tag_parse_expression if_test),
                                           (tag_parse_expression if_then),
                                           Const(Void)
                                          )
    | _ -> raise X_syntax_error

  and tag_parse_lambda rest_of_lambda =
    let get_var = function Symbol(v) -> v | _ -> raise X_syntax_error
    in let parse_rest_of_lambda arglist exprs_sequence =
         match arglist with
         (* Lambda variadic *)
         | ImproperList([], Symbol(optional)) -> LambdaOpt ([], optional, exprs_sequence)
         (* Lambda optional *)
         | ImproperList(mandatory, Symbol(optional)) -> LambdaOpt ((List.map get_var mandatory), optional, exprs_sequence)
         (* Lambda simple *)
         | ProperList(mandatory) -> LambdaSimple ((List.map get_var mandatory), exprs_sequence)
         | _ -> raise X_syntax_error
    in match rest_of_lambda with
      | ProperList([]) -> raise X_syntax_error
      | ProperList(car :: []) -> raise X_syntax_error
      | ProperList(arglist :: exprs) -> parse_rest_of_lambda (pair_to_pairlist arglist) (tag_parse_seq exprs)
      | ImproperList(_) -> raise X_syntax_error

  and tag_parse_seq = function
    | _ -> raise X_not_yet_implemented (* TODO *)
  ;;

  (* *************** TAG-PARSER ***************** *)
  let tag_parse_expressions sexpr = List.map tag_parse_expression sexpr;;


end;; (* struct Tag_Parser *)

