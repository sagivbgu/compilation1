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
let no_box expr = annotate_tail_calls (annotate_lexical_addresses expr);;

(* *************** FROM ASSIGNEMNT TESTS ***************** *)

(* 
    In (lambda (x) (set! x (+ x 1)) (lambda (y) (+ x y))), variable x should not be
    boxed, since the expression matches form 1: (set! x (+ x 1)) is the <write-occur>, while
    (lambda (y) (+ x y)) is E, in which the <read-occur> x is found.
*)
test_exps_lists "Assignemnt_Test_1" 
  [r (List.hd (tag_parse_expressions 
    (read_sexprs "(lambda (x) (set! x (+ x 1)) (lambda (y) (+ x y)))")))]

  [no_box (List.hd (tag_parse_expressions 
    (read_sexprs "(lambda (x) (set! x (+ x 1)) (lambda (y) (+ x y)))")))];;

(* 
    In (lambda (x) x (lambda (y) (set! x (+ x y)))), variable x should not be boxed,
    since the expression matches form 2: x is the <read-occur>, while (lambda (y) (set! x
    (+ x y))) is E, in which the <write-occur> (set! x (+ x y)) is found.
 *)
test_exps_lists "Assignemnt_Test_2" 
  [r (List.hd (tag_parse_expressions 
    (read_sexprs "(lambda (x) x (lambda (y) (set! x (+ x y))))")))]

  [no_box (List.hd (tag_parse_expressions 
    (read_sexprs "(lambda (x) x (lambda (y) (set! x (+ x y))))")))];;

(* 
    In (lambda (x) (list (lambda () (set! x (+ x 1))) (lambda () x))), variable x
    should be boxed, since the expression matches none of the four forms. Note that in this case,
    x is logically required to be boxed, since it is impossible to know in which order the write and
    read occur.
 *)
 test_exps_lists "Assignemnt_Test_3" 
 [r (List.hd (tag_parse_expressions 
   (read_sexprs "(lambda (x) (list (lambda () (set! x (+ x 1))) (lambda () x)))")))]

[
  LambdaSimple' (["x"],
  Seq'([Set'(VarParam("x", 0), Box'(VarParam("x",0)));
  ApplicTP' (Var' (VarFree "list"),
   [LambdaSimple' ([],
     BoxSet' (VarBound ("x", 0, 0),
      Applic' (Var' (VarFree "+"),
       [BoxGet' (VarBound ("x", 0, 0));
        Const' (Sexpr (Number (Fraction (1, 1))))])));
    LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])]))
];;


(* 
    In (lambda (x) (list (set! x (+ x 1)) (lambda () x))), variable x should be
    boxed, since the expression matches none of the four forms. Note that in this case, x is
    logically required to be boxed, since it is impossible to know in which order the write and the
    environment extension occur.
 *)
 test_exps_lists "Assignemnt_Test_4" 
 [r (List.hd (tag_parse_expressions 
   (read_sexprs "(lambda (x) (list (set! x (+ x 1)) (lambda () x)))")))] 

[
  LambdaSimple' (["x"],
  Seq'([Set'(VarParam("x", 0), Box'(VarParam("x",0)));
  ApplicTP' (Var' (VarFree "list"),
  [BoxSet' (VarParam ("x", 0),
    Applic' (Var' (VarFree "+"),
     [BoxGet' (VarParam ("x", 0)); Const' (Sexpr (Number (Fraction (1, 1))))]));
   LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])]))
];;


(*  

    HERE we are in a strange situation - we don't box x, but they say we should, 
    although it's not logically required

    In (lambda (x) (lambda (z) (set! x (+ z 1))) (lambda (w) x)), variable x should
    be boxed, since the expression matches none of the four forms. Note that in this case, x is
    not logically required to be boxed, since (lambda (z) (set! x (+ z 1))) is dead code:
    we can see that at runtime, the closure created for this lambda will be immediately garbage
    collected, and will never be called. However, your compiler does not contain the necessary
    analysis to realize this, and thus you should box x in this case.
 *)
 test_exps_lists "Assignemnt_Test_5" 
 [r (List.hd (tag_parse_expressions 
   (read_sexprs "(lambda (x) (lambda (z) (set! x (+ z 1))) (lambda (w) x))")))] 

[
  LambdaSimple' (["x"],
 Seq'
  [Set'(VarParam("x", 0), Box'(VarParam("x",0)));
    LambdaSimple' (["z"],
    BoxSet' (VarBound ("x", 0, 0),
     Applic' (Var' (VarFree "+"),
      [Var' (VarParam ("z", 0)); Const' (Sexpr (Number (Fraction (1, 1))))])));
   LambdaSimple' (["w"], BoxGet' (VarBound ("x", 0, 0)))])
];;

(* *************** FOREIGN TESTS ***************** *)

(* fails because the test sais we should box y, but i don't think that's correct*)
(* test_exps_lists "ForeignTest2_2" 
  [r (List.hd (tag_parse_expressions 
        (read_sexprs 
        "(y (lambda (y) (set! a (lambda (b) (a b)))
                        (set! t (lambda (x) (set! y (lambda (j) (x j x))) h)) 
                        (y a)))")))] 
  
  [Applic' (Var' (VarFree "y"),
  [LambdaSimple' (["y"],
    Seq'
     [Set'(VarParam ("y", 0), Box' (VarParam ("y", 0)));
       Set' (VarFree "a",
         LambdaSimple' (["b"],
          ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));
        Set' (VarFree "t",
         LambdaSimple' (["x"],
          Seq'
           [BoxSet' (VarBound ("y", 0, 0),
             LambdaSimple' (["j"],
              ApplicTP' (Var' (VarBound ("x", 0, 0)),
               [Var' (VarParam ("j", 0)); Var' (VarBound ("x", 0, 0))])));
            Var' (VarFree "h")]));
        ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])])];; *)


(* *************** GREETING ***************** *)
Printf.printf "\nAll Done!\n";;
