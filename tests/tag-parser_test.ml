(* #use "reader.ml";; *)
#use "tag-parser.ml";;
open Reader;;
open Tag_Parser;;

let eq sexp_list1 sexp_list2 =
  let s1 = List.hd sexp_list1 in
  let s2 = List.hd sexp_list2 in
  sexpr_eq s1 s2;;


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

(* *************** ORIGINAL TESTS ***************** *)
test_exps_lists "Example1" (p [Bool(true)]) [Const (Sexpr (Bool true))];;

test_exps_lists "Boolean1" (p [Bool(true)]) [Const (Sexpr (Bool true))];;

test_exps_lists "If1" (p [Pair (Symbol "if", Pair (Bool true, Pair (Bool true, Pair (Bool false, Nil))))]) [If (Const (Sexpr (Bool true)), Const (Sexpr (Bool true)), Const (Sexpr (Bool false)))];;

test_exps_lists "Or1" (p [Pair (Symbol "or", Nil)]) ([(Const(Sexpr(Bool(false))))]);;

(* ( letrec ( ( a 1/1 )  )  2/1 ) *)
test_exps_lists "Letrec1" (p ([Pair (Symbol "letrec",Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)), Nil),
                                                           Pair (Number (Fraction (2, 1)), Nil)))]))
  ([Applic(LambdaSimple (["a"],Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));
                                   Const (Sexpr (Number (Fraction (2, 1))))]),
           [Const (Sexpr (Symbol "whatever"))])]);;

(* ( letrec ( ( a 2/1 )  )  1/1 a ) *)
test_exps_lists "Letrec2" (p ([Pair (Symbol "letrec",Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (2, 1)), Nil)), Nil),
                                                           Pair (Number (Fraction (1, 1)), Pair (Symbol "a", Nil))))]))
  ([Applic(LambdaSimple (["a"],Seq[
       Set (Var "a", Const (Sexpr (Number (Fraction (2, 1)))));
       Const (Sexpr (Number (Fraction (1, 1))));
       Var "a"]),
           [Const (Sexpr (Symbol "whatever"))])]);;

(* ( letrec ( ( a 1/1 )  ( b 1/1 )  ( c 3/1 )  )  1/1 a ) *)
test_exps_lists "Letrec3" (p ([Pair (Symbol "letrec",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),
                                                                Pair (Pair (Symbol "b", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (3, 1)), Nil)), Nil))),
                                                          Pair (Number (Fraction (1, 1)), Pair (Symbol "a", Nil))))]))
  ([Applic(LambdaSimple (["a"; "b"; "c"], Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));
                                              Set (Var "b", Const (Sexpr (Number (Fraction (1, 1)))));
                                              Set (Var "c", Const (Sexpr (Number (Fraction (3, 1)))));
                                              Const (Sexpr (Number (Fraction (1, 1))));
                                              Var "a"]
                        ),
           [Const (Sexpr (Symbol "whatever"));
            Const (Sexpr (Symbol "whatever"));
            Const (Sexpr (Symbol "whatever"))])]);;

(* (letrec () 2 4) *)
test_exps_lists "Letrec4" (p ([Pair (Symbol "letrec",
                                     Pair (Nil,
                                           Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (4, 1)), Nil))))]))
  ([Applic(LambdaSimple ([], Seq[
       Const (Sexpr (Number (Fraction (2, 1))));
       Const (Sexpr (Number (Fraction (4, 1))))]
     ),
           [])]);;

(* (letrec () 2) *)
test_exps_lists "Letrec5" (p ([Pair (Symbol "letrec",
                                     Pair (Nil,
                                           Pair (Number (Fraction (2, 1)), Nil)))]))
  ([Applic(LambdaSimple ([], Const (Sexpr (Number (Fraction (2, 1))))), [])]);;

(* (letrec ((a 1) (b 3)) b) *)
test_exps_lists "Letrec6" (p ([Pair (Symbol "letrec",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),
                                                                Pair (Pair (Symbol "b", Pair (Number (Fraction (3, 1)), Nil)), Nil)),
                                                          Pair (Symbol "b", Nil)))]))
  ([Applic(LambdaSimple (["a"; "b"], Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));
                                         Set (Var "b", Const (Sexpr (Number (Fraction (3, 1)))));
                                         Var "b"]
                        ),
           [Const (Sexpr (Symbol "whatever"));
            Const (Sexpr (Symbol "whatever"))])]);;

test_exps_lists "MultipleSexprs1" (p [Bool(true); Bool(false)]) [Const (Sexpr (Bool true)); Const (Sexpr (Bool false))];;

test_exps_lists "Lambda1" (p [Pair (Symbol "lambda", Pair (Nil, Pair (Bool true, Nil)))]) [LambdaSimple([], Const(Sexpr(Bool(true))))];;

(* *************** FOREIGN TESTS ***************** *)
test_exps_lists "test01" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)), Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)), Nil)), Pair (Symbol "y", Nil)))])) ([Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number ((Fraction(2,1)))))])]);;
test_exps_lists "test02" (p ([Pair (Symbol "let", Pair (Nil, Pair (Number (Fraction(10,1)), Nil)))])) ([Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction(10,1))))), [])]);;
test_exps_lists "test03" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)), Nil), Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Fraction(1,1))))])]);;
test_exps_lists "test04" (p ([Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))), Nil)))])) ([Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))]),[])]);;
test_exps_lists "test05" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction(3,1)), Nil)), Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)), Nil)), Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))])) ([Applic (LambdaSimple (["a"; "b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction(3,1)))); Const (Sexpr (Number (Fraction(10,1))))])]);;
test_exps_lists "test07" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil), Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);;

test_exps_lists "test08" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Nil)))])) ([Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 1.23))))),[Const (Sexpr (String "asd"))])]);;
test_exps_lists "test09" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Pair (Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction(-1,1)), Nil))), Nil))), Nil)))])) ([Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction(-1,1))))])]),[Const (Sexpr (String "asd"))])]);;
test_exps_lists "test10" (p ([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);;
(* Quasiquote tests (below) *)
test_exps_lists "test22" (p ([Pair (Symbol "lambda", Pair (Nil, Pair (Number (Fraction(10,1)), Nil))) ])) ([LambdaSimple ([], Const (Sexpr (Number (Fraction(10,1)))))]);;
test_exps_lists "test23" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Symbol "a", Nil)))  ])) ([LambdaSimple (["a"; "b"], Var "a")]);;
test_exps_lists "test24" (p ([Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Nil)))  ])) ([LambdaSimple (["a"], Var "a")]);;
test_exps_lists "test25" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))])) ([LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"]))]);;
test_exps_lists "test26" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "if",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"], If (Var "x", Var "y", Var "z"))]);;
test_exps_lists "test27" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"], Seq [Var "x"; Var "y"; Var "z"])]);;
test_exps_lists "test28" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "set", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))])) ([LambdaSimple (["x"; "y"], Applic (Var "set", [Var "x"; Var "y"]))]);;
test_exps_lists "test29" (p ([Pair (Symbol "lambda",Pair(Pair (Symbol "x",Pair (Symbol "y", Pair (Symbol "z", Pair (Symbol "w", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "x",Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "z", Pair (Symbol "w", Nil))), Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"; "w"],If (Var "x", Applic (Var "+", [Var "y"; Var "z"]),Applic (Var "+", [Var "z"; Var "w"])))]);;
test_exps_lists "test30" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair(Pair (Symbol "or",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)),    Nil))) ])) ([LambdaSimple (["x"; "y"], Applic (Var "x", [Var "y"; Var "z"]))]);; 

test_exps_lists "test31" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs", Seq [Var "x"; Var "y"; Var "vs"])]);;
test_exps_lists "test32" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Symbol "vs"),Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "vs", Nil))), Nil)))])) ([LambdaOpt (["x"], "vs", If (Var "x", Var "vs", Const Void))]);;
test_exps_lists "test33" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(2,1)))), Const (Sexpr (Number (Fraction(3,1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);;
test_exps_lists "test34" (p ([Pair (Symbol "lambda",Pair(Pair (Symbol "a",Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "vs")))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "c", Pair (Symbol "d", Nil))),Pair (Pair (Symbol "list", Pair (Symbol "vs", Nil)), Nil)))),Nil)))])) ([LambdaOpt (["a"; "b"; "c"; "d"], "vs",If (Applic (Var ">", [Var "a"; Var "b"]),Applic (Var "+", [Var "c"; Var "d"]), Applic (Var "list", [Var "vs"])))]);;
test_exps_lists "test35" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "b", Symbol "vs"),Pair(Pair (Symbol "begin",Pair (Symbol "b",Pair(Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction(10,1)), Nil))),Pair(Pair (Symbol "set",Pair (Symbol "b",Pair(Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Fraction(15,1)), Nil))),Nil))),Nil)))),Nil)))])) ([LambdaOpt (["b"], "vs",Seq[Var "b"; Def (Var "x", Const (Sexpr (Number (Fraction(10,1)))));Applic (Var "set",[Var "b"; Applic (Var "+", [Var "x"; Const (Sexpr (Number(Fraction(15,1))))])])])]);;
test_exps_lists "test36" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "vs")),Pair(Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(2,1)), Nil)),Pair(Pair (Symbol "else",Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))),Nil)),Nil)))),Nil)))])) ([LambdaOpt (["a"; "b"], "vs",If (Var "a", Const (Sexpr (Number (Fraction(1,1)))),If (Var "b", Const (Sexpr (Number (Fraction(2,1)))),Applic (Var "+", [Var "a"; Var "b"]))))]);;
test_exps_lists "test37" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Symbol "vs"), Pair (Symbol "vs", Nil)))   ])) ([LambdaOpt (["x"], "vs", Var "vs")]);;
(* A test with quasiquote *)
test_exps_lists "test39" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",If (Var "x", If (Var "y", Var "vs", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);;



test_exps_lists "test40" (p ([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)), Nil)),Pair (Symbol "y", Nil)))])) ([Applic(LambdaSimple (["x"],Applic (LambdaSimple (["y"], Var "y"), [Const (Sexpr (Number (Fraction(2,1))))])),[Const (Sexpr (Number (Fraction(1,1))))])]);;

test_exps_lists "test41" (p ([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "z", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(10,1)), Nil)), Nil)))))),Pair(Pair (Symbol "begin",Pair (Symbol "x",Pair (Symbol "y",Pair (Symbol "z",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))))),Nil)))])) ([Applic(LambdaSimple (["x"],Applic(LambdaSimple (["y"],Applic(LambdaSimple (["z"],Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Seq [Var "x"; Var "y"; Var "z"; Var "a"; Var "b"; Var "c"]),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
test_exps_lists "test42" (p ([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "d", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "e", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "f", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "g", Pair (Number (Fraction(10,1)), Nil)), Nil))))))),Pair(Pair (Symbol "and",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Pair (Symbol "d",Pair (Symbol "e", Pair (Symbol "f", Pair (Symbol "g", Nil)))))))),Nil)))])) ([Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Applic(LambdaSimple (["d"],Applic(LambdaSimple (["e"],Applic(LambdaSimple (["f"],Applic(LambdaSimple (["g"],If (Var "a",If (Var "b",If (Var "c",If (Var "d",If (Var "e",If (Var "f", Var "g", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
test_exps_lists "test43" (p ([Pair (Symbol "let*",Pair (Nil,Pair(Pair (Symbol "begin",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))]),[])]);;
test_exps_lists "test44" (p ([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)), Nil)),Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))])) ([Applic(LambdaSimple (["a"],Applic (LambdaSimple (["b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
test_exps_lists "test45" (p ([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil),Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);;
test_exps_lists "test46" (p ([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "t", Pair (Bool true, Nil)),Pair (Pair (Symbol "th", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "el", Pair (Number (Fraction(10,1)), Nil)), Nil))),Pair(Pair (Symbol "if",Pair (Bool true, Pair (Number (Fraction(10,1)), Pair (Number (Fraction(10,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple (["t"],Applic(LambdaSimple (["th"],Applic(LambdaSimple (["el"],If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Fraction(10,1)))),Const (Sexpr (Number (Fraction(10,1)))))),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Bool true))])]);;
test_exps_lists "test47" (p ([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 12.3), Nil))),Nil)))])) ([Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 12.3))))),[Const (Sexpr (String "asd"))])]);;
test_exps_lists "test48" (p ([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "begin",Pair(Pair (Symbol "define",Pair (Symbol "y", Pair (Number (Float 1.23), Nil))),Pair(Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction(10,1)), Nil))),Nil))),Nil)))])) ([Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction(10,1))))])]),[Const (Sexpr (String "asd"))])]);;
test_exps_lists "test49" (p ([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);;



test_exps_lists "test100" (p ([Pair (Symbol "quote", Pair(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000'))),Nil))])) ([Const(Sexpr(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000')))))]);; 

test_exps_lists "test99" (p ([Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil)), Nil))])) ([Const (Sexpr (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil))))]);;

test_exps_lists "test98" (p ([Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))])) ([Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))]);;
test_exps_lists "test97" (p ([Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))])) ([Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))]);;
test_exps_lists "test96" (p ([Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))])) ([Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))]);;
test_exps_lists "test95" (p ([Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))])) ([Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))]);;
test_exps_lists "test94" (p ([String ""])) ([Const (Sexpr (String ""))]);;
test_exps_lists "test93" (p ([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z")))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z"))))))]);;
test_exps_lists "test92" (p ([Pair (Symbol "quote",Pair(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1)))))))),Nil))])) ([Const(Sexpr(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1))))))))))]);;
test_exps_lists "test91" (p ([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(6,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f")))))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(6,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f"))))))))]);;
test_exps_lists "test90" (p ([Pair (Symbol "quote",Pair(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' ')))),Nil))])) ([Const(Sexpr(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' '))))))]);;
test_exps_lists "test89" (p ([Pair (Symbol "quote",Pair(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g"))))),Nil))])) ([Const(Sexpr(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g")))))))]);;

test_exps_lists "test88" (p ([Pair (Symbol "quote",Pair(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex", Pair (Char '\r', Pair (String "list", Nil)))))))),Nil))])) ([Const(Sexpr(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex",Pair (Char '\r', Pair (String "list", Nil))))))))))]);;
test_exps_lists "test87" (p ([Pair (Symbol "quote",Pair (Pair (String "should", Pair (String "be", String "list")), Nil))])) ([Const (Sexpr (Pair (String "should", Pair (String "be", String "list"))))]);;
test_exps_lists "test86" (p ([Pair (String "should not", Pair (String "be", Pair (String "list", Nil)))])) ([Applic (Const (Sexpr (String "should not")),[Const (Sexpr (String "be")); Const (Sexpr (String "list"))])]);;
test_exps_lists "test85" (p ([Pair (Symbol "quote",Pair(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000'))),Nil))])) ([Const(Sexpr(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000')))))]);;
test_exps_lists "test84" (p ([Pair (Symbol "quote",Pair(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e"))),Nil))])) ([Const(Sexpr(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e")))))]);;
test_exps_lists "test83" (p ([Pair (Symbol "quote",Pair(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil))),Nil))])) ([Const(Sexpr(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil)))))]);;
test_exps_lists "test82" (p ([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil)))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil))))))]);;
test_exps_lists "test81" (p ([Char '\n'])) ([Const (Sexpr (Char '\n'))]);;
test_exps_lists "test80" (p ([Pair (Symbol "quote", Pair (Pair (Char '\000', Nil), Nil))])) ([Const (Sexpr (Pair (Char '\000', Nil)))]);;

test_exps_lists "test79" (p ([Pair (Symbol "quote", Pair (Pair (Number (Float 0.123), Nil), Nil))])) ([Const (Sexpr (Pair (Number (Float 0.123), Nil)))]);;
test_exps_lists "test78" (p ([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535)))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535))))))]);;
test_exps_lists "test77" (p ([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555))))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555)))))))]);;
test_exps_lists "test76" (p ([Pair (Symbol "quote",Pair(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.)))),Nil))])) ([Const(Sexpr(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.))))))]);;
test_exps_lists "test75" (p ([Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Number (Float 3.4)), Nil))])) ([Const (Sexpr (Pair (Number (Float 1.2), Number (Float 3.4))))]);;
test_exps_lists "test74" (p ([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))))]);;
test_exps_lists "test73" (p ([Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))])) ([Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "", [Var "a"; Var "b"; Var "c"])))]);;
test_exps_lists "test72" (p ([Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction(1,1)), Pair (Number (Fraction(-1,1)), Nil)))),Nil)))])) ([Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction(1,1)))),Const (Sexpr (Number (Fraction(-1,1)))))))]);;
test_exps_lists "test70" (p ([Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))])) ([Def (Var "square",LambdaSimple (["x"], Applic (Var "", [Var "x"; Var "x"])))]);;
(* Quasiquote test *)

test_exps_lists "test55" (p ([Pair (Symbol "and", Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))))))))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))), Const (Sexpr (Number (Fraction(1,1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))]);;  

test_exps_lists "test54" (p ([Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))));Const (Sexpr (Number (Fraction(1,1))))])])]);; 
test_exps_lists "test53" (p ([Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil))])) ([Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])]);;
test_exps_lists "test52" (p ([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,2)), Pair (Number (Fraction(3,3)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(4,4)), Pair (Number (Fraction(5,5)), Pair (Number (Fraction(6,6)), Nil)))),Pair(Pair (Number (Fraction(7,7)), Pair (Number (Fraction(8,8)), Pair (Number (Fraction(9,9)), Nil))),Nil))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(2,2)))); Const (Sexpr (Number (Fraction(3,3))))],Seq[Const (Sexpr (Number (Fraction(4,4)))); Const (Sexpr (Number (Fraction(5,5))));Const (Sexpr (Number (Fraction(6,6))))])]);; 
test_exps_lists "test51" (p ([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))),Nil))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))));Const (Sexpr (Number (Fraction(1,1))))]))]);; 
test_exps_lists "test50" (p ([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Nil)))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],Const Void))]);;
test_exps_lists "test54" (p ([Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))));Const (Sexpr (Number (Fraction(1,1))))])])]);;

test_exps_lists "test11" (p ([Pair (Symbol "quasiquote",  Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil))  ])) ([Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])]);;

test_exps_lists "test12" (p ([Pair (Symbol "quasiquote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil))  ])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;
test_exps_lists "test13" (p ([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))])) ([Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;

test_exps_lists "test14" (p ([Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])]);;
test_exps_lists "test15" (p ([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))])) ([Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;
test_exps_lists "test16" (p ([Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
test_exps_lists "test17" (p ([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
test_exps_lists "test18" (p ([Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
test_exps_lists "test19 " (p ([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))])) ([Applic (Var "append", [Var "a"; Var "b"])]);; 
test_exps_lists "test20" (p ([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),Nil))])) ([Applic (Var "cons", [Var "a"; Var "b"])]);; 


test_exps_lists "test21 " (p ([Pair (Symbol "quasiquote",Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))])) ([Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])]);;

test_exps_lists "test38" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair(Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "y", Nil)), Nil)),Nil)),Nil),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",Applic(Applic (Var "cons",[Var "x"; Applic (Var "append", [Var "y"; Const (Sexpr Nil)])]),[]))]);;

test_exps_lists "test71" (p ([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Symbol "d"))),Pair(Pair (Symbol "quasiquote",Pair(Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),Pair(Pair (Symbol "b",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Pair(Pair (Symbol "c",Pair (Pair (Symbol "unquote", Pair (Symbol "c", Nil)), Nil)),Pair(Pair (Symbol "d",Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)), Nil)),Nil)))),Nil)),Nil)))])) ([LambdaOpt (["a"; "b"; "c"], "d",Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "a"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "b"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "c"));Applic (Var "cons", [Var "c"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "d"));Applic (Var "cons", [Var "d"; Const (Sexpr Nil)])]);Const (Sexpr Nil)])])])]))]);;

(* *************** GREETING ***************** *)
Printf.printf "\nAll Done!\n";;
