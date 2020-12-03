#use "tag-parser.ml";;
open Tag_Parser;;

let unread_number n =
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
  | Pair(car, cdr) -> Printf.sprintf "(%s . %s)" (unread car) (unread cdr)
and unread_list = function
  | Pair(a, b) -> Printf.sprintf " %s%s" (unread a) (unread_list b)
  | Nil -> ")"
  | sexpr -> Printf.sprintf "%s)" (unread sexpr);;

let untag expr = 
  let rec untag_rec expr is_nested = 
    match expr with
    | Const(Sexpr(s)) -> unread s
    | Const(Void) when is_nested -> "#<void>"
    | Const(Void) -> ""
    | Var(name) -> unread (Symbol(name))
    | If(test, dit, dif) -> Printf.sprintf "(if %s %s %s)" (untag_nested test) (untag_nested dit) (untag_nested dif)
    | Seq(exprs) -> Printf.sprintf "(begin %s)" (untag_list exprs)
    | Or(exprs) ->  Printf.sprintf "(or %s)" (untag_list exprs)
    | Set(expr1, expr2) -> Printf.sprintf "(set! %s %s)" (untag_nested expr1) (untag_nested expr2)
    | Def(expr1, expr2) -> Printf.sprintf "(define %s %s)" (untag_nested expr1) (untag_nested expr2)
    | LambdaSimple(args, expr) -> Printf.sprintf "(lambda (%s) %s)" (String.concat " " args) (untag_nested expr)
    | LambdaOpt([], arg, expr) -> Printf.sprintf "(lambda %s %s)" arg (untag_nested expr)
    | LambdaOpt(args, arg, expr) -> Printf.sprintf "(lambda (%s . %s) %s)" (String.concat " " args) arg (untag_nested expr)
    | Applic(expr, args) -> Printf.sprintf "(%s %s)" (untag_nested expr) (untag_list args) 
  and untag_nested expr = untag_rec expr true 
  and untag_list exprs = String.concat " " (List.map untag_nested exprs) in
  untag_rec expr false

let print_exprs exprs = 
  let exprs = List.map untag exprs in
  Printf.printf "%s\n" (String.concat "\n" exprs);;

let test_exp res expected =
  if expr_eq res expected
  then true
  else false;;

exception TestFail_Result_Ended_Before_Expected;;  
exception Test_Fail_No_Match;;

let test_exps_lists name lst1 lst2 = 
  let func = 
    (fun acc b -> 
       match acc with
       | [] -> Printf.printf "Test: %s -> Fail\n\tResult Ended, But Expected: %s\n" name (untag b);
         raise TestFail_Result_Ended_Before_Expected
       | a::res1 -> if (test_exp a b)
         then (res1)
         else ([];
               Printf.printf "Test: %s -> Fail:\n\tGot: %s\n\tExpected: %s\n\t" name (untag a) (untag b);
               raise Test_Fail_No_Match)
    ) in
  List.fold_left func lst1 lst2;
  Printf.printf "Test: %s -> Success" name;;


let p = tag_parse_expressions;;

test_exps_lists "Example1" (p [Bool(true)]) [Const (Sexpr (Bool true))];;

test_exps_lists "Boolean1" (p [Bool(true)]) [Const (Sexpr (Bool true))];;

test_exps_lists "If1" (p [Pair (Symbol "if", Pair (Bool true, Pair (Bool true, Pair (Bool false, Nil))))]) [If (Const (Sexpr (Bool true)), Const (Sexpr (Bool true)), Const (Sexpr (Bool false)))];;

test_exps_lists "MultipleSexprs1" (p [Bool(true); Bool(false)]) [Const (Sexpr (Bool true)); Const (Sexpr (Bool false))];;

(* Don't forget to test lambda with 0 parameters: (lamnda () #t) *)

Printf.printf "\nAll Done!\n";;
