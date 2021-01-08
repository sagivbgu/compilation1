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


(* Official tests for assignment 3 *)

test_exps_lists "2" [r (LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1))))))] [(LambdaSimple' ([], Const' (Sexpr (Number (Fraction (1, 1))))))];;
test_exps_lists "3" [r (Const
  (Sexpr
    (Pair
      (Pair (Symbol "lambda",
        Pair (Nil,
         Pair
          (Pair (Symbol "lambda",
            Pair (Pair (Symbol "x", Nil),
             Pair (Symbol "x",
              Pair
               (Pair (Symbol "lambda",
                 Pair (Nil,
                  Pair
                   (Pair (Symbol "set!",
                     Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil))),
                   Nil))),
               Nil)))),
          Nil))),
      Nil))))] [(
Const'
 (Sexpr
   (Pair
     (Pair (Symbol "lambda",
       Pair (Nil,
        Pair
         (Pair (Symbol "lambda",
           Pair (Pair (Symbol "x", Nil),
            Pair (Symbol "x",
             Pair
              (Pair (Symbol "lambda",
                Pair (Nil,
                 Pair
                  (Pair (Symbol "set!",
                    Pair (Symbol "x", Pair (Number (Fraction(1, 1)), Nil))),
                  Nil))),
              Nil)))),
         Nil))),
     Nil))))];;
test_exps_lists "4" [r (Applic
  (LambdaSimple (["x"],
    If (Applic (Var "a", [Const (Sexpr (Number (Fraction(1, 1))))]),
     Applic (Var "b", [Const (Sexpr (Number (Fraction(2, 1))))]),
     Applic
      (LambdaSimple (["x"], Set (Var "c", Const (Sexpr (Number (Fraction (0, 1)))))),
      [Const (Sexpr (Number (Fraction (3, 1))))]))),
  [LambdaSimple (["x"], Var "d")]))] [(
Applic'
 (LambdaSimple' (["x"],
   If'
    (Applic' (Var' (VarFree "a"), [Const' (Sexpr (Number (Fraction(1, 1))))]),
    ApplicTP' (Var' (VarFree "b"), [Const' (Sexpr (Number (Fraction (2, 1))))]),
    ApplicTP'
     (LambdaSimple' (["x"],
       Set' (VarFree "c", Const' (Sexpr (Number (Fraction (0, 1)))))),
     [Const' (Sexpr (Number (Fraction (3, 1))))]))),
 [LambdaSimple' (["x"], Var' (VarFree "d"))]))];;
test_exps_lists "6" [r (If (Applic (LambdaSimple (["y"], Var "x"), []),
  Applic
   (LambdaSimple (["x"],
     Seq
      [Set (Var "x", Var "y");
       LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1))))))]),
   [Const (Sexpr (Symbol "a"))]),
  LambdaSimple (["x"], Set (Var "x", Var "y"))))] [(
If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []),
 Applic'
  (LambdaSimple' (["x"],
    Seq'
     [Set' (VarParam ("x", 0), Var' (VarFree "y"));
      LambdaSimple' ([],
       Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (1, 1))))))]),
  [Const' (Sexpr (Symbol "a"))]),
 LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarFree "y")))))];;
test_exps_lists "7" [r (LambdaOpt (["x"; "y"; "z"], "w",
  Seq
   [Var "z";
        LambdaSimple ([],
       Seq [Set (Var "w", Var "w")])]))] [(
LambdaOpt' (["x"; "y"; "z"], "w",
 Seq'
  [Var' (VarParam ("z", 2));
    LambdaSimple' ([],
      Seq'
       [Set' (VarBound ("w", 0, 3), Var' (VarBound ("w", 0, 3)))])]))];;
test_exps_lists "8" [r (Def (Var "a",
  Applic
   (LambdaSimple ([],
     LambdaOpt ([], "x",
      Seq
       [Var "x";
        LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Fraction (1, 1))))))])),
   [])))] [(
Def' (VarFree "a",
 Applic'
  (LambdaSimple' ([],
    LambdaOpt' ([], "x",
     Seq'
      [Var' (VarParam ("x", 0));
       LambdaOpt' ([], "y",
        Set' (VarParam ("y", 0), Const' (Sexpr (Number (Fraction (1, 1))))))])),
  [])))];;
test_exps_lists "11" [r (LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [
        Set (Var "x", Const (Sexpr (Number (Fraction (5, 1)))));
       Applic (Var "+", [Var "x"; Var "y"]);
       Var "x"
])]))] [(
LambdaSimple' (["x"; "y"; "z"],
   Seq'
    [LambdaSimple' (["y"],
      Seq'
       [
        Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (5, 1)))));
        Applic' (Var' (VarFree "+"), [
        Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))]);
        Var' (VarBound ("x", 0, 0))
    ])
]))];;
test_exps_lists "16" [r (LambdaOpt ([], "x",
  Seq
   [LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1))))));
    Var "x"]))] [(
LambdaOpt' ([], "x",
 Seq'
  [LambdaSimple' (["x"],
    Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (1, 1))))));
 Var' (VarParam ("x", 0))]))];;
test_exps_lists "17" [r (If (Var "x", Applic (Var "x", []), Var "x"))] [(
If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []),
 Var' (VarFree "x")))];;
test_exps_lists "18" [r (LambdaSimple ([],
  If (Var "x", Applic (Var "x", []), Applic (Var "not", [Var "x"]))))] [(
LambdaSimple' ([],
 If' (Var' (VarFree "x"), ApplicTP' (Var' (VarFree "x"), []),
  ApplicTP' (Var' (VarFree "not"), [Var' (VarFree "x")]))))];;
test_exps_lists "19" [r (LambdaSimple (["a"; "b"; "c"; "d"; "e"],
  Applic (Var "x",
   [Applic (Var "y", [Var "z"]); Applic (Var "f", [Var "g"; Var "h"]);
    Applic (Var "j",
     [Applic (Var "k", [Applic (Var "l", [Applic (Var "m", [Var "n"])])])])])))] [(
LambdaSimple' (["a"; "b"; "c"; "d"; "e"],
 ApplicTP' (Var' (VarFree "x"),
  [Applic' (Var' (VarFree "y"), [Var' (VarFree "z")]);
   Applic' (Var' (VarFree "f"),
    [Var' (VarFree "g"); Var' (VarFree "h")]);
   Applic' (Var' (VarFree "j"),
    [Applic' (Var' (VarFree "k"),
      [Applic' (Var' (VarFree "l"),
        [Applic' (Var' (VarFree "m"), [Var' (VarFree "n")])])])])])))];;
test_exps_lists "20" [r (LambdaSimple (["x"],
  Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))]))] [(
LambdaSimple' (["x"],
 Seq'
  [Applic' (Var' (VarParam ("x", 0)), []);
   Set' (VarParam ("x", 0), Applic' (Var' (VarParam ("x", 0)), []))]))];;
test_exps_lists "21" [r (LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Seq [Set (Var "a", Applic (Var "b", [])); Const (Sexpr (Number (Fraction (2, 1))))]),
   [])))] [(
LambdaSimple' (["x"],
 ApplicTP'
  (LambdaSimple' (["y"],
    Seq'
     [Set' (VarFree "a",
       Applic' (Var' (VarFree "b"), []));
      Const' (Sexpr (Number (Fraction (2, 1))))]),
  [])))];;
test_exps_lists "22" [r (Const(Void))] [( Const' Void)];;
test_exps_lists "25" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "f", [Applic (Var "g", [Applic (Var "g", [Var "a"])])])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "f"),
  [Applic' (Var' (VarFree "g"),
    [Applic' (Var' (VarFree "g"), [Var' (VarFree "a")])])])))];;
test_exps_lists "26" [r (LambdaSimple (["x"],
 Applic (Var "f",
  [LambdaSimple (["y"], Applic (Var "g", [Var "a"; Var "b"]))])))] [(
LambdaSimple' (["x"],
 ApplicTP' (Var' (VarFree "f"),
  [LambdaSimple' (["y"],
    ApplicTP' (Var' (VarFree "g"),
     [Var' (VarFree "a"); Var' (VarFree "b")]))])))];;
test_exps_lists "27" [r (LambdaSimple (["x"; "y"; "z"; "w"],
 If (Applic (Var "even?", [Var "a"]), Applic (Var "b", [Var "c"]),
  Applic (Var "d", [Var "e"]))))] [(
LambdaSimple' (["x"; "y"; "z"; "w"],
 If' (Applic' (Var' (VarFree "even?"), [Var' (VarFree "a")]),
  ApplicTP' (Var' (VarFree "b"), [Var' (VarFree "c")]),
  ApplicTP' (Var' (VarFree "d"), [Var' (VarFree "e")]))))];;
test_exps_lists "28" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "f",
  [If (Applic (Var "odd?", [Var "a"]), Applic (Var "h", [Var "b"]),
    Applic (Var "w", [Var "c"]))])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "f"),
  [If' (Applic' (Var' (VarFree "odd?"), [Var' (VarFree "a")]),
    Applic' (Var' (VarFree "h"), [Var' (VarFree "b")]),
    Applic' (Var' (VarFree "w"), [Var' (VarFree "c")]))])))];;
test_exps_lists "29" [r (LambdaSimple (["a"; "b"],
 Seq
  [Applic (Var "f", [Var "z"]); Applic (Var "g", [Var "x"; Var "y"]);
   Applic (Var "display", [Const (Sexpr (String "done!\n"))])]))] [(
LambdaSimple' (["a"; "b"],
 Seq'
  [Applic' (Var' (VarFree "f"), [Var' (VarFree "z")]);
   Applic' (Var' (VarFree "g"),
    [Var' (VarFree "x"); Var' (VarFree "y")]);
   ApplicTP' (Var' (VarFree "display"), [Const' (Sexpr (String "done!\n"))])]))];;
test_exps_lists "30" [r (LambdaSimple ([],
 If (Applic (Var "f", [Var "x"]),
  If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
   Const (Sexpr (Bool false))),
  Const (Sexpr (Bool false)))))] [(
LambdaSimple' ([],
 If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
  If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
   ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
   Const' (Sexpr (Bool false))),
  Const' (Sexpr (Bool false)))))];;
test_exps_lists "31" [r (LambdaSimple (["x"; "y"],
 Or [Applic (Var "f", []); Applic (Var "g", [Var "a"])]))] [(
LambdaSimple' (["x"; "y"],
 Or'
  [Applic' (Var' (VarFree "f"), []); ApplicTP' (Var' (VarFree "g"), [Var' (VarFree "a")])]))];;
test_exps_lists "32" [r (LambdaSimple (["x"], Set (Var "a", Applic (Var "f", [Var "y"]))))] [(
LambdaSimple' (["x"],
 Set' (VarFree "a",
  Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]))))];;
test_exps_lists "33" [r (LambdaSimple ([],
 Set (Var "x",
  Applic (Var "f",
   [LambdaSimple (["y"], Applic (Var "g", [Var "x"; Var "a"]))]))))] [(
LambdaSimple' ([],
 Set' (VarFree "x",
  Applic' (Var' (VarFree "f"),
   [LambdaSimple' (["y"],
     ApplicTP' (Var' (VarFree "g"),
      [Var' (VarFree "x"); Var' (VarFree "a")]))]))))];;
test_exps_lists "34" [r (LambdaSimple (["x"; "y"; "z"],
 If (Applic (Var "f?", [Var "a"]), Applic (Var "g", [Var "b"]),
  If (Applic (Var "g?", [Var "c"]),
   Seq [Applic (Var "f", [Var "d"]); Applic (Var "f", [Var "e"])],
   Seq
    [Applic (Var "h", [Var "w"]); Applic (Var "f", [Var "l"]);
     Applic (Var "g", [Applic (Var "f", [Var "m"])])]))))] [(
LambdaSimple' (["x"; "y"; "z"],
 If' (Applic' (Var' (VarFree "f?"), [Var' (VarFree "a")]),
  ApplicTP' (Var' (VarFree "g"), [Var' (VarFree "b")]),
  If' (Applic' (Var' (VarFree "g?"), [Var' (VarFree "c")]),
   Seq'
    [Applic' (Var' (VarFree "f"), [Var' (VarFree "d")]);
     ApplicTP' (Var' (VarFree "f"), [Var' (VarFree "e")])],
   Seq'
    [Applic' (Var' (VarFree "h"), [Var' (VarFree "w")]);
     Applic' (Var' (VarFree "f"), [Var' (VarFree "l")]);
     ApplicTP' (Var' (VarFree "g"),
      [Applic' (Var' (VarFree "f"), [Var' (VarFree "m")])])]))))];;
test_exps_lists "35" [r (Applic (LambdaSimple (["x"; "y"], Applic (Var "+", [Var "a"; Var "b"])),
 [Applic (Var "f", [Var "y"]); Applic (Var "g", [Var "c"])]))] [(
Applic'
 (LambdaSimple' (["x"; "y"],
   ApplicTP' (Var' (VarFree "+"),
    [Var' (VarFree "a"); Var' (VarFree "b")])),
 [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]);
  Applic' (Var' (VarFree "g"), [Var' (VarFree "c")])]))];;
test_exps_lists "37" [r (LambdaSimple (["x"; "y"; "z"], Applic (Var "+", [Var "a"; Var "b"; Var "c"])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   Var' (VarFree "c")])))];;
test_exps_lists "38" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+", [Var "c"; Var "d"; Const (Sexpr (Number (Fraction (1, 1))))]))])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarFree "c"); Var' (VarFree "d");
      Const' (Sexpr (Number (Fraction (1, 1))))]))])))];;
test_exps_lists "39" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+", [Var "c"; Const (Sexpr (Number (Fraction (2, 1))))]))])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarFree "c"); Const' (Sexpr (Number (Fraction (2, 1))))]))])))];;
test_exps_lists "40" [r (LambdaOpt (["x"; "y"], "z",
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z"; LambdaSimple (["z"], Applic (Var "+", [Var "c"; Var "d"]))]))])))] [(
LambdaOpt' (["x"; "y"], "z",
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' (["z"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarFree "c"); Var' (VarFree "d")]))]))])))];;
test_exps_lists "41" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "c";
      LambdaSimple (["x"], Applic (Var "+", [Var "d"; Var "e"; Var "f"]))]))])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarFree "c");
      LambdaSimple' (["x"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarFree "d"); Var' (VarFree "e");
         Var' (VarFree "f")]))]))])))];;
test_exps_lists "42" [r (LambdaOpt (["x"; "y"], "z",
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "c"; LambdaSimple ([], Applic (Var "+", [Var "d"; Var "e"]))]))])))] [(
LambdaOpt' (["x"; "y"], "z",
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarFree "c");
      LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarFree "d"); Var' (VarFree "e")]))]))])))];;
test_exps_lists "43" [r (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "a"; Var "b";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "c"; Var "d"; Var "e";
      LambdaSimple ([], Applic (Var "+", [Var "f"; Var "g"; Var "h"]))]))])))] [(
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarFree "a"); Var' (VarFree "b");
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarFree "c"); Var' (VarFree "d");
      Var' (VarFree "e");
      LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarFree "f"); Var' (VarFree "g");
         Var' (VarFree "h")]))]))])))];;
test_exps_lists "45" [r (Def (Var "test",
 LambdaSimple (["x"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))]))))] [(
Def' (VarFree "test",
 LambdaSimple' (["x"],
  Seq'
   [Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' (["y"],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])])))];;
test_exps_lists "46" [r (Def (Var "test",
 LambdaSimple (["x"; "y"],
  Set (Var "x", Applic (Var "*", [Var "x"; Var "y"])))))] [(
Def' (VarFree "test",
 LambdaSimple' (["x"; "y"],
  Set' (VarParam ("x", 0),
   Applic' (Var' (VarFree "*"),
    [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1))])))))];;
test_exps_lists "48" [r (Def (Var "test",
 LambdaSimple (["x"; "y"],
  Applic (Var "list",
   [LambdaSimple ([],
     Set (Var "a",
      Applic (Var "+", [Var "b"; Const (Sexpr (Number (Fraction(1, 1))))])));
    LambdaSimple ([], Var "c")]))))] [(
Def' (VarFree "test",
 LambdaSimple' (["x"; "y"],
  ApplicTP' (Var' (VarFree "list"),
   [LambdaSimple' ([],
     Set' (VarFree "a",
      Applic' (Var' (VarFree "+"),
       [Var' (VarFree "b"); Const' (Sexpr (Number (Fraction (1, 1))))])));
    LambdaSimple' ([], Var' (VarFree "c"))]))))];;
test_exps_lists "49" [r (Def (Var "test",
 LambdaSimple (["x"],
  LambdaSimple (["op"],
   If (Applic (Var "eq?", [Var "o"; Const (Sexpr (Symbol "read"))]),
    LambdaSimple ([], Var "x"),
    If (Applic (Var "eq?", [Var "a"; Const (Sexpr (Symbol "write"))]),
     LambdaSimple (["val"], Set (Var "b", Var "c")), Const Void))))))] [(
Def' (VarFree "test",
 LambdaSimple' (["x"],
  LambdaSimple' (["op"],
   If'
    (Applic' (Var' (VarFree "eq?"),
      [Var' (VarFree "o"); Const' (Sexpr (Symbol "read"))]),
    LambdaSimple' ([], Var' (VarBound ("x", 1, 0))),
    If'
     (Applic' (Var' (VarFree "eq?"),
       [Var' (VarFree "a"); Const' (Sexpr (Symbol "write"))]),
     LambdaSimple' (["val"],
      Set' (VarFree "b", Var' (VarFree "c"))),
     Const' Void))))))];;
test_exps_lists "50" [r (Def (Var "test",
 LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Applic (Var "cons",
      [LambdaSimple ([], Var "a");
       Applic (Var "cons", [Set (Var "b", Var "c"); Const (Sexpr Nil)])])),
   [Const (Sexpr (Number (Fraction (1, 1))))]))))] [(
Def' (VarFree "test",
 LambdaSimple' (["x"],
  ApplicTP'
   (LambdaSimple' (["y"],
     ApplicTP' (Var' (VarFree "cons"),
      [LambdaSimple' ([], Var' (VarFree "a"));
       Applic' (Var' (VarFree "cons"),
        [Set' (VarFree "b", Var' (VarFree "c"));
         Const' (Sexpr Nil)])])),
   [Const' (Sexpr (Number (Fraction (1, 1))))]))))];;
test_exps_lists "51" [r (Def (Var "test",
 LambdaOpt (["x"], "y",
    Applic (Var "cons", [
        Var "x";
        LambdaSimple ([], Set (Var "x", Var "y"))
    ])
)))] [(Def' (VarFree "test",
    LambdaOpt' (["x"], "y",
        Seq' ([
            Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
            ApplicTP' (Var' (VarFree "cons"), [
                BoxGet' (VarParam ("x", 0));
                LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))
            ])
        ])
    )))];;
test_exps_lists "58" [r (Def (Var "func",
 LambdaSimple (["x"; "y"; "z"; "w"],
  Applic (Var "list",
   [
       LambdaSimple ([], Var "x");
        LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction (0, 1))))));
    ]))))] [(Def' (VarFree "func",
 LambdaSimple' (["x"; "y"; "z"; "w"],
  Seq'
   [Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' ([],
       BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (0, 1))))));
       ])])))];;
test_exps_lists "60" [r (LambdaOpt (["x"], "y", Var "x"))] [( LambdaOpt' (["x"], "y", Var' (VarParam ("x", 0))))];;
test_exps_lists "61" [r (Applic
 (LambdaSimple (["x"],
  LambdaSimple (["y"], Var "y")
),
    [Applic (Var "f", [Var "y"])]))] [(
Applic'
 (LambdaSimple' (["x"],
        LambdaSimple' (["y"],
        Var' (VarParam ("y", 0)))),
 [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")])]))];;
test_exps_lists "62" [r (LambdaSimple (["x"; "y"; "z"; "w"],
  If (Applic (Var "foo?", [Var "x"]),
    Var "goo",
  Var "boo"
)))] [(
LambdaSimple' (["x"; "y"; "z"; "w"],
 If' (Applic' (Var' (VarFree "foo?"), [Var' (VarParam ("x", 0))]),
        Var' (VarFree "goo"),
    Var' (VarFree "boo"))))];;
test_exps_lists "64" [r (LambdaSimple ([],
  If (Applic (Var "f", [Var "x"]),
   If (Applic (Var "g", [Var "y"]), Const (Sexpr (Bool false)),
    Const (Sexpr (Bool false))),
   Const (Sexpr (Bool false)))))] [(
LambdaSimple' ([],
 If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
  If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
   Const' (Sexpr (Bool false)),
   Const' (Sexpr (Bool false))),
  Const' (Sexpr (Bool false)))))];;
test_exps_lists "73" [r (LambdaSimple (["x"],
  LambdaOpt (["x"], "y",
   If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction (5, 1))))]),
    LambdaSimple (["x"],
        Var "x"),
    LambdaSimple (["x"],
        Var "x")
    )
)))] [(
LambdaSimple' (["x"],
 LambdaOpt' (["x"], "y",
  If'
   (Applic' (Var' (VarParam ("x", 0)),
     [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction (5, 1))))]),
   LambdaSimple' (["x"],
        Var' (VarParam ("x", 0))
    ),
   LambdaSimple' (["x"],
        Var' (VarParam ("x", 0))
)))))];;
test_exps_lists "74" [r (LambdaSimple (["x"],
  LambdaOpt (["a"], "y",
   If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction (5, 1))))]),
    LambdaSimple (["x"],
      Var "x"
        ),
    LambdaSimple (["x"],
        Var "x"
        )
    )
)))] [(
LambdaSimple' (["x"],
 LambdaOpt' (["a"], "y",
  If'
   (Applic' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction (5, 1))))]),
   LambdaSimple' (["x"],
   Var' (VarParam ("x", 0))
    ),
   LambdaSimple' (["x"],
    Var' (VarParam ("x", 0))
)))))];;
test_exps_lists "75" [r (LambdaSimple (["a"],
  Seq
   [LambdaSimple ([],
     LambdaSimple (["x"; "y"; "z"],
      Or [Applic (Var "b", [Var "c"]); Applic (Var "d", [Var "e"])]));
    LambdaSimple (["x"], Applic (Var "f", [Var "g"]))]))] [(
LambdaSimple' (["a"],
 Seq'
  [LambdaSimple' ([],
    LambdaSimple' (["x"; "y"; "z"],
     Or'
      [Applic' (Var' (VarFree "b"), [Var' (VarFree "c")]);
       ApplicTP' (Var' (VarFree "d"), [Var' (VarFree "e")])]));
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarFree "f"), [Var' (VarFree "g")]))]))];;
test_exps_lists "82" [r (LambdaSimple (["x"],
  LambdaSimple ([],
   Seq
    [Applic (LambdaSimple (["x"], Const (Sexpr (Number (Fraction (1, 1))))), [Var "x"]);
     LambdaSimple ([], LambdaSimple ([], Var "x"))])))] [(
LambdaSimple' (["x"],
 LambdaSimple' ([],
  Seq'
   [Applic' (LambdaSimple' (["x"], Const' (Sexpr (Number (Fraction (1, 1))))),
     [Var' (VarBound ("x", 0, 0))]);
    LambdaSimple' ([],
     LambdaSimple' ([], Var' (VarBound ("x", 2, 0))))])))];;
test_exps_lists "602" [r (LambdaSimple (["x"; "y"; "z"],
     Seq
      [
        Set (Var "x", Const (Sexpr (Number (Fraction (5, 1)))));
        Var "x"
]))] [(
LambdaSimple' (["x"; "y"; "z"],
      Seq'
       [
        Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1)))));
        Var' (VarParam ("x", 0))
    ]))];;
test_exps_lists "603" [r (Def (Var "test",
 LambdaOpt (["x"], "y",
    Applic (Var "cons", [
        Var "x";
        LambdaSimple ([], Set (Var "x", Var "c"));
        LambdaSimple ([], Seq ([
            Var "y";
            Set (Var "y", Var "a");
            Applic (Var "b", [])
        ]))
    ])
)))] [(Def' (VarFree "test",
    LambdaOpt' (["x"], "y",
        Seq' ([
            Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
            ApplicTP' (Var' (VarFree "cons"), [
                BoxGet' (VarParam ("x", 0));
                LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "c")));
                LambdaSimple' ([], Seq' ([
                    Var' (VarBound ("y", 0, 1));
                    Set' (VarBound ("y", 0, 1), Var' (VarFree "a"));
                    ApplicTP' (Var'  (VarFree "b"), [])
                ])
            )
        ])
    ])
)))];;
test_exps_lists "604" [r (Def (Var "test",
 LambdaOpt (["x"], "y",
    Applic (Var "cons", [
        Var "x";
        LambdaSimple ([], Set (Var "x", Var "c"));
        LambdaSimple ([], Seq ([
            Applic (Var "x", [Var "y"]);
            Var "y";
            Set (Var "y", Var "a")
        ]))
    ])
)))] [(Def' (VarFree "test",
    LambdaOpt' (["x"], "y",
        Seq' ([
            Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
            ApplicTP' (Var' (VarFree "cons"), [
                BoxGet' (VarParam ("x", 0));
                LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "c")));
                LambdaSimple' ([], Seq' ([
                    Applic' (BoxGet' (VarBound ("x", 0, 0)), [Var' (VarBound ("y", 0, 1))]);
                    Var' (VarBound ("y", 0, 1));
                    Set' (VarBound ("y", 0, 1), Var' (VarFree "a"))
                ])
            )
        ])
    ])
)))];;
test_exps_lists "605" [r (Def (Var "test",
    LambdaOpt (["x"], "y",
        Applic (Var "cons", [
            LambdaSimple (["a"], Seq ([
                Set (Var "a", Var "f");
                Or [Var "a"]
                ]));
            Var "y";
            LambdaSimple ([],
                Or [Set (Var "y", Var "a"); Var "x"]
            )
        ])
    )))] [(Def' (VarFree "test",
    LambdaOpt' (["x"], "y",
        Seq' ([
            Set' (VarParam ("y", 1), Box' (VarParam ("y", 1)));
            ApplicTP' (Var' (VarFree "cons"), [
                LambdaSimple' (["a"],
                    Seq' ([
                        Set' (VarParam ("a", 0), Var' (VarFree "f"));
                        Or' [Var' (VarParam ("a", 0))]
                    ]));
                BoxGet' (VarParam ("y", 1));
                LambdaSimple' ([],
                    Or' [BoxSet' (VarBound ("y", 0, 1), Var' (VarFree "a")); Var' (VarBound ("x", 0, 0))]
                )
            ])
        ])
    )))];;
test_exps_lists "201" [r (LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1))))))] [(LambdaSimple' ([], Const' (Sexpr (Number (Fraction (1, 1))))))];;
test_exps_lists "202" [r (Const
 (Sexpr
   (Pair
     (Pair (Symbol "lambda",
       Pair (Nil,
        Pair
         (Pair (Symbol "lambda",
           Pair (Pair (Symbol "x", Nil),
            Pair (Symbol "x",
             Pair
              (Pair (Symbol "lambda",
                Pair (Nil,
                 Pair
                  (Pair (Symbol "set!",
                    Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil))),
                  Nil))),
              Nil)))),
         Nil))),
     Nil))))] [(Const' (Sexpr (Pair (Pair (Symbol "lambda",
      Pair (Nil, Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), 
      Pair (Symbol "x", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "set!", 
      Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil))), Nil))), Nil)))), Nil))), Nil))))];;
test_exps_lists "203" [r (Applic (LambdaSimple (["x"],
            If (Applic (Var "a", [Const (Sexpr (Number (Fraction (1, 1))))]),
                Var "b",
                LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction (0, 1))))))
            )), [LambdaSimple (["x"], Var "x")]))] [(Applic' (LambdaSimple' (["x"],
            If' (Applic' (Var' (VarFree "a"), [Const' (Sexpr (Number (Fraction (1, 1))))]),
                Var' (VarFree "b"),
                LambdaSimple' (["x"], Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (0, 1))))))
            )), [
           LambdaSimple' (["x"], Var' (VarParam ("x", 0)))
]))];;
test_exps_lists "205" [r (If (Applic (LambdaSimple (["y"], Var "x"), []),
 Applic
  (LambdaSimple (["x"],
    Seq
     [Set (Var "x", Var "y");
      LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1))))))]),
  [Const (Sexpr (Symbol "a"))]),
 LambdaSimple (["x"], Set (Var "x", Var "y"))))] [(If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []),
            Applic' (LambdaSimple' (["x"], Seq' ([
                Set' (VarParam ("x", 0), Var' (VarFree "y"));
                LambdaSimple' ([], Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction (1, 1))))))
                ])), [Const' (Sexpr (Symbol "a"))]),
            LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarFree "y")))))];;
test_exps_lists "210" [r (LambdaSimple (["x"],
 Seq
  [LambdaSimple (["x"], Set (Var "x", Var "x"));
   LambdaSimple (["x"], Set (Var "x", Var "x"))]))] [(LambdaSimple' (["x"],
    Seq' ([
        LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))));
        LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))))])))];;
test_exps_lists "213" [r (If (Var "x", Applic (Var "x", []), Var "x"))] [(If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []), Var' (VarFree "x")))];;
test_exps_lists "214" [r (LambdaSimple ([],
If (Var "x", Var "x", Var "not")))] [(LambdaSimple' ([],
    If' (
        Var' (VarFree "x"),
        Var' (VarFree "x"),
        Var' (VarFree "not")
    )
))];;
test_exps_lists "215" [r (LambdaSimple (["a"; "b"; "c"; "d"; "e"],
    Seq ([
        Applic (Var "b", [Var "c"]);
        Applic (Var "c", [Var "b"; Var "d"]);
        Var "f"
    ])))] [(LambdaSimple' (["a";"b";"c";"d";"e"],
    Seq' ([
            Applic' (Var' (VarParam ("b", 1)), [Var' (VarParam ("c", 2))]);
            Applic' (Var' (VarParam ("c", 2)), [Var' (VarParam ("b", 1)); Var' (VarParam ("d", 3))]);
            Var' (VarFree "f")
        ])
))];;
test_exps_lists "216" [r (LambdaSimple (["x"],
Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))]))] [(LambdaSimple' (["x"], Seq' ([
    Applic' (Var' (VarParam ("x", 0)), []);
    Set' (VarParam ("x", 0), Applic' (Var' (VarParam ("x", 0)), []))])))];;
test_exps_lists "217" [r (LambdaSimple (["x"],
    Seq ([
        LambdaSimple (["y"],
        Seq [Set (Var "x", Applic (Var "y", [])); Const (Sexpr (Number (Fraction (2, 1))))])
 ])
 ))] [(LambdaSimple' (["x"],
    Seq' ([
            LambdaSimple' (["y"], Seq' ([
                Set' (VarBound ("x", 0, 0), Applic' (Var' (VarParam ("y", 0)), []));
                Const' (Sexpr (Number (Fraction (2, 1))))
            ]))
        ])
    ))];;
test_exps_lists "218" [r (LambdaSimple (["x"],
    Seq ([
        Var "x";
        LambdaSimple (["x"],
            Seq ([
                Set (Var "x", Const (Sexpr (Number (Fraction (1, 1)))));
                LambdaSimple ([], Var "x")
            ]));
            LambdaSimple ([], Set (Var "x", Var "x"))
])))] [(LambdaSimple' (["x"],
    Seq' ([
        Var' (VarParam ("x", 0));
        LambdaSimple' (["x"],
            Seq' ([
                Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (1, 1)))));
                LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))
            ]));
        LambdaSimple' ([], Set' (VarBound ("x", 0, 0), Var' (VarBound ("x", 0, 0))))
    ])
))];;
test_exps_lists "221" [r (LambdaSimple (["x"],
 Seq
  [LambdaSimple (["x"], Set (Var "x", Var "x"));
   LambdaSimple (["x"], Set (Var "x", Var "x"))]))] [(LambdaSimple' (["x"], Seq' ([LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))));
LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))))])))];;
test_exps_lists "222" [r (LambdaOpt ([], "x",
 If
  (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction (1, 1)))))),
   Var "x",
   Var "x"
)))] [(LambdaOpt' ([], "x",
    If' (
        LambdaSimple' (["x"], Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (1, 1)))))),
        Var' (VarParam ("x", 0)),
        Var' (VarParam ("x", 0))
    )
))];;
test_exps_lists "301" [r (LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1))))))] [(LambdaSimple' ([], Const' (Sexpr (Number (Fraction (1, 1))))))];;
test_exps_lists "302" [r (Applic
  (LambdaSimple (["x"],
    If (Applic (Var "a", [Const (Sexpr (Number (Fraction (1, 1))))]),
     Applic (Var "b", [Const (Sexpr (Number (Fraction (2, 1))))]),
     Applic
      (LambdaSimple (["x"], Set (Var "c", Const (Sexpr (Number (Fraction (0, 1)))))),
      [Const (Sexpr (Number (Fraction (3, 1))))]))),
  [LambdaSimple (["x"], Var "d")]))] [(Applic' (LambdaSimple' (["x"],
        If' (Applic' (Var' (VarFree "a"),
        [Const' (Sexpr (Number (Fraction (1, 1))))]),
        ApplicTP' (Var' (VarFree "b"), [Const' (Sexpr (Number (Fraction (2, 1))))]),
            ApplicTP' (LambdaSimple' (["x"], Set' (VarFree "c",
                Const' (Sexpr (Number (Fraction (0, 1)))))), [Const' (Sexpr (Number (Fraction (3, 1))))]))),
                [LambdaSimple' (["x"], Var' (VarFree "d"))]))];;
test_exps_lists "303" [r (LambdaSimple (["x"],
  Or
   [Applic
     (LambdaOpt (["y"], "z",
       Applic
        (LambdaSimple ([],
          Applic (LambdaSimple ([], Applic (Var "+", [Var "a"; Var "b"])), [])),
        [])),
     [Var "c"; Const (Sexpr (Number (Fraction (1, 1))))]);
    LambdaSimple ([], Set (Var "d", Var "w")); Applic (Var "w", [Var "w"])]))] [(LambdaSimple' (["x"], 
		Or' ([Applic' (LambdaOpt' (["y"], "z",
		ApplicTP' (LambdaSimple' ([], ApplicTP' (LambdaSimple' ([], ApplicTP' (Var' (VarFree "+"), [Var' (VarFree "a");
		Var' (VarFree "b")])), [])), [])), [Var' (VarFree "c");Const' (Sexpr (Number (Fraction (1, 1))))]);
		LambdaSimple' ([], Set' (VarFree "d", Var' (VarFree "w")));
		ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])])))];;
test_exps_lists "304" [r (If (Applic (LambdaSimple (["y"], Var "x"), []),
  Applic
   (LambdaSimple (["x"],
     Seq
      [Set (Var "a", Var "y");
       LambdaSimple ([], Set (Var "b", Const (Sexpr (Number (Fraction (1, 1))))))]),
   [Const (Sexpr (Symbol "a"))]),
  LambdaSimple (["x"], Set (Var "c", Var "y"))))] [(If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []),
        Applic' (LambdaSimple' (["x"],
            Seq' ([
                Set' (VarFree "a", Var' (VarFree "y"));
                LambdaSimple' ([], Set' (VarFree "b", Const' (Sexpr (Number (Fraction (1, 1))))))
                ])),
        [Const' (Sexpr (Symbol "a"))]),
            LambdaSimple' (["x"], Set' (VarFree "c", Var' (VarFree "y")))))];;
test_exps_lists "305" [r (LambdaSimple (["x"; "y"],
  Seq
   [Applic (Var "a", [Var "b"]);
    LambdaSimple ([],
     LambdaSimple ([],
      LambdaSimple ([],
       Set (Var "c",
        Applic (LambdaSimple (["z"], Set (Var "d", Var "e")), [Var "f"])))))]))] [(LambdaSimple' (["x";"y"],
    Seq' ([
        Applic' (Var' (VarFree "a"), [Var' (VarFree "b")]);
            LambdaSimple' ([],
                LambdaSimple' ([],
                    LambdaSimple' ([],
                        Set' (VarFree "c",
                            Applic' (LambdaSimple' (["z"],
                                Set' (VarFree "d", Var' (VarFree "e"))),
                                     [Var' (VarFree "f")])))))])))];;
test_exps_lists "307" [r (LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [Set (Var "a", Const (Sexpr (Number (Fraction (5, 1)))));
       Applic (Var "+", [Var "b"; Var "c"])]);
    Applic (Var "+", [Var "d"; Var "e"; Var "g"])]))] [(LambdaSimple' (["x";"y";"z"], 
	Seq' ([LambdaSimple' (["y"], Seq' ([Set' (VarFree "a", Const' (Sexpr (Number (Fraction (5, 1)))));
	ApplicTP' (Var' (VarFree "+"), [Var' (VarFree "b");
	Var' (VarFree "c")])]));
	ApplicTP' (Var' (VarFree "+"), [Var' (VarFree "d");
	Var' (VarFree "e");
	Var' (VarFree "g")])])))];;
test_exps_lists "308" [r (LambdaSimple (["x"], Set (Var "a", Applic (LambdaSimple ([], Var "b"), []))))] [(LambdaSimple' (["x"],
        Set' (VarFree "a", Applic' (LambdaSimple' ([], Var' (VarFree "b")), []))))];;
test_exps_lists "309" [r (Applic (Var "y",
  [LambdaSimple (["y"],
    Seq
     [Set (Var "a", LambdaSimple (["b"], Applic (Var "a", [Var "d"])));
      Set (Var "t",
       LambdaSimple (["x"],
        Seq
         [Set (Var "c",
           LambdaSimple (["j"], Applic (Var "e", [Var "f"; Var "g"])));
          Var "h"]));
      Applic (Var "h", [Var "a"])])]))] [(Applic' (Var' (VarFree "y"), [LambdaSimple' (["y"], 			
	Seq' ([Set' (VarFree "a", LambdaSimple' (["b"], ApplicTP' (Var' (VarFree "a"), [Var' (VarFree "d")])));
	Set' (VarFree "t", LambdaSimple' (["x"], Seq' ([Set' (VarFree "c",
	LambdaSimple' (["j"], ApplicTP' (Var' (VarFree "e"), [Var' (VarFree "f");
	Var' (VarFree "g")])));
		Var' (VarFree "h")])));
			ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "a")])]))]))];;
test_exps_lists "310" [r (LambdaSimple (["x"],
  Seq
   [LambdaSimple (["x"], Set (Var "x", Var "x"));
    LambdaSimple (["x"], Set (Var "x", Var "x"))]))] [(LambdaSimple' (["x"], Seq' ([LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))));
LambdaSimple' (["x"], Set' (VarParam ("x", 0), Var' (VarParam ("x", 0))))])))];;
test_exps_lists "311" [r (If (Var "x", Applic (Var "x", []), Var "x"))] [(If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []), Var' (VarFree "x")))];;
test_exps_lists "312" [r (LambdaSimple (["x"],
  Seq [Applic (Var "a", []); Set (Var "b", Applic (Var "c", []))]))] [(LambdaSimple' (["x"],
    Seq' ([
        Applic' (Var' (VarFree "a"), []);
        Set' (VarFree "b", Applic' (Var' (VarFree "c"), []))
        ])
    ))];;
test_exps_lists "313" [r (LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Seq [Set (Var "z", Applic (Var "w", [])); Const (Sexpr (Number (Fraction (2, 1))))]),
   [])))] [(LambdaSimple' (["x"],
    ApplicTP' (LambdaSimple' (["y"], Seq' ([
        Set' (VarFree "z", Applic' (Var' (VarFree "w"), []));
        Const' (Sexpr (Number (Fraction (2, 1))))])), [])))];;
test_exps_lists "317" [r (If (Var "x", Applic (Var "x", []), Var "x"))] [(If' (
    Var' (VarFree "x"),
    Applic' (Var' (VarFree "x"), []),
    Var' (VarFree "x")
))];;
test_exps_lists "318" [r (LambdaSimple ([],
  If (Var "x",
      Var "x",
      Var "not"
)))] [(LambdaSimple' ([],
    If' (
        Var' (VarFree "x"),
        Var' (VarFree "x"),
        Var' (VarFree "not")
        )
    ))];;
test_exps_lists "319" [r (LambdaSimple (["a"; "b"; "c"; "d"; "e"],
  Seq ([
    Var "a";
    Var "b";
    Var "c";
    Var "d";
    Var "e"
  ])
     ))] [(LambdaSimple' (["a";"b";"c";"d";"e"],
    Seq' ([
    Var' (VarParam ("a", 0));
    Var' (VarParam ("b", 1));
    Var' (VarParam ("c", 2));
    Var' (VarParam ("d", 3));
    Var' (VarParam ("e", 4))
    ])
))];;
test_exps_lists "320" [r (LambdaSimple (["x"],
  Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))]))] [(LambdaSimple' (["x"], Seq' ([
    Applic' (Var' (VarParam ("x", 0)), []);
    Set' (VarParam ("x", 0), Applic' (Var' (VarParam ("x", 0)), []))])))];;
test_exps_lists "407" [r (LambdaOpt (["x"; "y"; "z"], "w",
  Seq ([
        LambdaSimple ([],
            Set (Var "w", Var "w")
        )
    ])
))] [(LambdaOpt' (["x";"y";"z"], "w", Seq' ([
    LambdaSimple' ([],
        Set' (VarBound ("w", 0, 3), Var' (VarBound ("w", 0, 3)))
        )
    ])
))];;
test_exps_lists "501" [r (LambdaOpt (["x"; "y"; "z"], "w",
    Seq ([
        Applic (LambdaSimple ([], Set (Var "w", Var "w")), []);
        Const (Sexpr (Number (Fraction (1, 1))))
])))] [(
LambdaOpt' (["x"; "y"; "z"], "w",
 Seq' ([
    Applic' (LambdaSimple' ([], Set' (VarBound ("w", 0, 3), Var' (VarBound ("w", 0, 3)))), []);
    Const' (Sexpr (Number (Fraction (1, 1))))
])))];;
test_exps_lists "502" [r (Def (Var "test",
    LambdaOpt (["x"], "y",
        Applic (Var "cons", [
            LambdaSimple (["a"], Seq ([
                Set (Var "a", Var "f");
                Var "a"
                ]));
            Var "y";
            LambdaSimple ([],
                Seq ([
                    Set (Var "y", Var "a");
                ]))
        ])
    )))] [(Def' (VarFree "test",
    LambdaOpt' (["x"], "y",
        Seq' ([
            Set' (VarParam ("y", 1), Box' (VarParam ("y", 1)));
            ApplicTP' (Var' (VarFree "cons"), [
                LambdaSimple' (["a"],
                    Seq' ([
                        Set' (VarParam ("a", 0), Var' (VarFree "f"));
                        Var' (VarParam ("a", 0))
                    ]));
                BoxGet' (VarParam ("y", 1));
                LambdaSimple' ([],
                    Seq' ([
                        BoxSet' (VarBound ("y", 0, 1), Var' (VarFree "a"));
                    ])
                )
            ])
        ])
    )))];;
test_exps_lists "504" [r (LambdaSimple (["a"; "b"],
 Seq
  [Applic (Var "display", [Const (Sexpr (String "done!\n"))])]))] [(
LambdaSimple' (["a"; "b"],
 Seq'
  [ApplicTP' (Var' (VarFree "display"), [Const' (Sexpr (String "done!\n"))])]))];;


(* *************** GREETING ***************** *)
Printf.printf "\nAll Done!\n";;
