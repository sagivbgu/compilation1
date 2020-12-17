#use "semantic-analyser.ml";;
open Reader;;
open Tag_Parser;;
open Semantics;;

let eq sexp_list1 sexp_list2 =
  let s1 = List.hd sexp_list1 in
  let s2 = List.hd sexp_list2 in
  sexpr_eq s1 s2;;

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

(* type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);; *)

let untag expr = 
  let rec untag_rec expr is_nested = 
    match expr with
    | Const'(Sexpr(s)) -> unread s
    | Const'(Void) when is_nested -> "#<void>"
    | Const'(Void) -> ""
    | Var'(var) -> untag_var var
    | Box'(var) -> Printf.sprintf "Box(%s)" (untag_var var)
    | BoxGet'(var) -> Printf.sprintf "BoxGet(%s)" (untag_var var)
    | BoxSet'(var, rhs) -> Printf.sprintf "BoxSet(%s, %s)" (untag_var var) (untag_nested rhs)
    | If'(test, dit, dif) -> Printf.sprintf "(if %s %s %s)" (untag_nested test) (untag_nested dit) (untag_nested dif)
    | Seq'(exprs) -> Printf.sprintf "(begin %s)" (untag_list exprs)
    | Or'(exprs) ->  Printf.sprintf "(or %s)" (untag_list exprs)
    | Set'(var, expr2) -> Printf.sprintf "(set! %s %s)" (untag_var var) (untag_nested expr2)
    | Def'(var, expr2) -> Printf.sprintf "(define %s %s)" (untag_var var) (untag_nested expr2)
    | LambdaSimple'(args, expr) -> Printf.sprintf "(lambda (%s) %s)" (String.concat " " args) (untag_nested expr)
    | LambdaOpt'([], arg, expr) -> Printf.sprintf "(lambda %s %s)" arg (untag_nested expr)
    | LambdaOpt'(args, arg, expr) -> Printf.sprintf "(lambda (%s . %s) %s)" (String.concat " " args) arg (untag_nested expr)
    | Applic'(expr, args) -> Printf.sprintf "(%s %s)" (untag_nested expr) (untag_list args) 
    | ApplicTP'(expr, args) -> Printf.sprintf "(TP: %s %s)" (untag_nested expr) (untag_list args) 
  and untag_nested expr = untag_rec expr true 
  and untag_var = function
    | VarFree(name) -> Printf.sprintf "VarFree(%s)" name
    | VarParam(name, i) -> Printf.sprintf "VarParam(%s, %d)" name i
    | VarBound(name, i, j) -> Printf.sprintf "VarBound(%s, %d, %d)" name i j
  and untag_list exprs = String.concat " \n" (List.map untag_nested exprs) in
  untag_rec expr false

let print_exprs exprs = 
  let exprs = List.map untag exprs in
  Printf.printf "%s\n" (String.concat "\n" exprs);;

let test_exp res expected =
  if expr'_eq res expected
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
               Printf.printf "Test: %s -> Fail:\n\nGot: %s\n\nExpected: %s\n\n" name (untag a) (untag b);
               raise Test_Fail_No_Match)
    ) in
  List.fold_left func lst1 lst2;
  Printf.printf "Test: %s -> Success" name;;

let r = run_semantics;;

(* *************** FOREIGN TESTS ***************** *)

(* The problem here - we boxed x, although test said it should not *)
test_exps_lists "ForeignTest1_1" 
  [r (List.hd (tag_parse_expressions 
        (read_sexprs 
        "(lambda (x y z) (lambda (y) (set! x 5) (+ x y)) (+ x y z))")))] 
  
  [LambdaSimple' (["x"; "y"; "z"],
  Seq'
   [
    LambdaSimple' (["y"],
       Seq'
        [Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(5,1)))));
         ApplicTP' (Var' (VarFree "+"),
          [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])]);
      ApplicTP' (Var' (VarFree "+"),
       [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
        Var' (VarParam ("z", 2))])])];;

(* The problem here - we boxed x, although test said it should not *)
test_exps_lists "ForeignTest1_2"
    [r (List.hd (tag_parse_expressions 
        (read_sexprs 
          "(lambda (x) (set! x ((lambda () x))))")))]

    [LambdaSimple' (["x"],
    Set' (VarParam ("x", 0),
     Applic' (LambdaSimple' ([], Var' (VarBound ("x", 0, 0))), [])))];;

(* *************** GREETING ***************** *)
Printf.printf "\nAll Done!\n";;
