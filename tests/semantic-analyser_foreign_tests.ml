#use "semantic-analyser.ml";;
open Semantics;;

let rec test_exp' input expected = 
  try expr'_eq (run_semantics input) expected
  with _ -> false;;
(* ------------------------------------------------------------------- *)
let test0 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Fraction(5,1)))));
       Applic (Var "+", [Var "x"; Var "y"])]);
    Applic (Var "+", [Var "x"; Var "y"; Var "z"])])) ) (
LambdaSimple' (["x"; "y"; "z"],
 Seq'
  [
   LambdaSimple' (["y"],
      Seq'
       [Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(5,1)))));
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])]);
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       Var' (VarParam ("z", 2))])])) ;;
let test1 = test_exp' ( (LambdaSimple (["x"], Set (Var "x", Applic (LambdaSimple ([], Var "x"), [])))) ) (
LambdaSimple' (["x"],
   Set' (VarParam ("x", 0),
    Applic' (LambdaSimple' ([], Var' (VarBound ("x", 0, 0))), [])))) ;;
let test2 = test_exp' ( (Applic (Var "y",
  [LambdaSimple (["y"],
    Seq
     [Set (Var "a", LambdaSimple (["b"], Applic (Var "a", [Var "b"])));
      Set (Var "t",
       LambdaSimple (["x"],
        Seq
         [Set (Var "y",
           LambdaSimple (["j"], Applic (Var "x", [Var "j"; Var "x"])));
          Var "h"]));
      Applic (Var "y", [Var "a"])])])) ) (
Applic' (Var' (VarFree "y"),
 [LambdaSimple' (["y"],
     Seq'
      [Set'( (VarFree "a"),
        LambdaSimple' (["b"],
         ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));
       Set'( (VarFree "t"),
        LambdaSimple' (["x"],
         Seq'
          [Set' (VarBound ("y", 0, 0),
            LambdaSimple' (["j"],
             ApplicTP' (Var' (VarBound ("x", 0, 0)),
              [Var' (VarParam ("j", 0)); Var' (VarBound ("x", 0, 0))])));
           Var' (VarFree "h")]));
       ApplicTP' (Var' (VarParam ("y", 0)), [Var' (VarFree "a")])])])) ;;
let test3 = test_exp' ( (LambdaSimple (["x"],
  Seq
   [LambdaSimple (["x"], Set (Var "x", Var "x"));
    LambdaSimple (["x"], Set (Var "x", Var "x"))])) ) (
LambdaSimple' (["x"],
 Seq'
  [LambdaSimple' (["x"],
    Set'( (VarParam ("x", 0)), Var' (VarParam ("x", 0))));
   LambdaSimple' (["x"],
    Set'( (VarParam ("x", 0)), Var' (VarParam ("x", 0))))])) ;;
let test4 = test_exp' ( (LambdaSimple (["x"; "y"],
  Seq
   [LambdaSimple ([], Set (Var "x", Var "y"));
    LambdaSimple ([], Set (Var "y", Var "x"))])) ) (
LambdaSimple' (["x"; "y"],
   Seq'
    [LambdaSimple' ([],
      Set' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))));
     LambdaSimple' ([],
      Set' (VarBound ("y", 0, 1), Var' (VarBound ("x", 0, 0))))])) ;;
let test5 = test_exp' ( (LambdaOpt ([], "x",
  Seq
   [LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction(1,1))))));
    Applic (Var "car", [Var "x"])])) ) (
LambdaOpt' ([], "x",
 Seq'
  [LambdaSimple' (["x"],
    Set'( (VarParam ("x", 0)), Const' (Sexpr (Number (Fraction(1,1))))));
   ApplicTP' (Var' (VarFree "car"), [Var' (VarParam ("x", 0))])])) ;;
let test6 = test_exp' ( (If (Var "x", Applic (Var "x", []), Var "x")) ) (
If' (Var' (VarFree "x"), Applic' (Var' (VarFree "x"), []),
 Var' (VarFree "x"))) ;;
let test7 = test_exp' ( (LambdaSimple ([],
  If (Var "x", Applic (Var "x", []), Applic (Var "not", [Var "x"])))) ) (
LambdaSimple' ([],
 If' (Var' (VarFree "x"), ApplicTP' (Var' (VarFree "x"), []),
  ApplicTP' (Var' (VarFree "not"), [Var' (VarFree "x")])))) ;;
let test8 = test_exp' ( (LambdaSimple (["a"; "b"; "c"; "d"; "e"],
  Applic (Var "a",
   [Applic (Var "b", [Var "c"]); Applic (Var "c", [Var "b"; Var "d"]);
    Applic (Var "a",
     [Applic (Var "b", [Applic (Var "c", [Applic (Var "d", [Var "e"])])])])]))) ) (
LambdaSimple' (["a"; "b"; "c"; "d"; "e"],
 ApplicTP' (Var' (VarParam ("a", 0)),
  [Applic' (Var' (VarParam ("b", 1)), [Var' (VarParam ("c", 2))]);
   Applic' (Var' (VarParam ("c", 2)),
    [Var' (VarParam ("b", 1)); Var' (VarParam ("d", 3))]);
   Applic' (Var' (VarParam ("a", 0)),
    [Applic' (Var' (VarParam ("b", 1)),
      [Applic' (Var' (VarParam ("c", 2)),
        [Applic' (Var' (VarParam ("d", 3)), [Var' (VarParam ("e", 4))])])])])]))) ;;
let test9 = test_exp' ( (LambdaSimple (["x"],
  Seq [Applic (Var "x", []); Set (Var "x", Applic (Var "x", []))])) ) (
LambdaSimple' (["x"],
 Seq'
  [Applic' (Var' (VarParam ("x", 0)), []);
   Set'( (VarParam ("x", 0)), Applic' (Var' (VarParam ("x", 0)), []))])) ;;
let test10 = test_exp' ( (LambdaSimple ([], Const (Sexpr (Number (Fraction(1,1)))))) ) (LambdaSimple' ([], Const' (Sexpr (Number (Fraction(1,1)))))) ;;
let test11 = test_exp' ( (LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Seq [Set (Var "x", Applic (Var "y", [])); Const (Sexpr (Number (Fraction(2,1))))]),
   []))) ) (
LambdaSimple' (["x"],
 ApplicTP'
  (LambdaSimple' (["y"],
    Seq'
     [Set'( (VarBound ("x", 0, 0)),
       Applic' (Var' (VarParam ("y", 0)), []));
      Const' (Sexpr (Number (Fraction(2,1))))]),
  []))) ;;
let test12 = test_exp' ( (Const(Void)) ) ( Const' Void) ;;
let test13 = test_exp' ( (LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Fraction(1,1)))));
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])) ) (
LambdaSimple' (["x"],
   Seq'
    [Var' (VarParam ("x", 0));
     LambdaSimple' (["x"],
        Seq'
         [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
          LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]);
     LambdaSimple' ([],
      Set' (VarBound ("x", 0, 0), Var' (VarBound ("x", 0, 0))))])) ;;
let test14 = test_exp' ( (LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "y", Var "x");
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])) ) (
LambdaSimple' (["x"],
   Seq'
    [Var' (VarParam ("x", 0));
     LambdaSimple' (["x"],
      Seq'
       [Set'( (VarFree "y"), Var' (VarParam ("x", 0)));
        LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]);
     LambdaSimple' ([],
      Set' (VarBound ("x", 0, 0), Var' (VarBound ("x", 0, 0))))])) ;;
let test15 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "f", [Applic (Var "g", [Applic (Var "g", [Var "x"])])]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "f"),
  [Applic' (Var' (VarFree "g"),
    [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])])]))) ;;
let test16 = test_exp' ( (LambdaSimple (["x"],
 Applic (Var "f",
  [LambdaSimple (["y"], Applic (Var "g", [Var "x"; Var "y"]))]))) ) (
LambdaSimple' (["x"],
 ApplicTP' (Var' (VarFree "f"),
  [LambdaSimple' (["y"],
    ApplicTP' (Var' (VarFree "g"),
     [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))]))]))) ;;
let test17 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "w"],
 If (Applic (Var "even?", [Var "x"]), Applic (Var "y", [Var "w"]),
  Applic (Var "z", [Var "w"])))) ) (
LambdaSimple' (["x"; "y"; "z"; "w"],
 If' (Applic' (Var' (VarFree "even?"), [Var' (VarParam ("x", 0))]),
  ApplicTP' (Var' (VarParam ("y", 1)), [Var' (VarParam ("w", 3))]),
  ApplicTP' (Var' (VarParam ("z", 2)), [Var' (VarParam ("w", 3))])))) ;;
let test18 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "f",
  [If (Applic (Var "odd?", [Var "x"]), Applic (Var "h", [Var "y"]),
    Applic (Var "w", [Var "z"]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "f"),
  [If' (Applic' (Var' (VarFree "odd?"), [Var' (VarParam ("x", 0))]),
    Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
    Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))]))) ;;
let test19 = test_exp' ( (LambdaSimple (["a"; "b"],
 Seq
  [Applic (Var "f", [Var "a"]); Applic (Var "g", [Var "a"; Var "b"]);
   Applic (Var "display", [Const (Sexpr (String "done!\n"))])])) ) (
LambdaSimple' (["a"; "b"],
 Seq'
  [Applic' (Var' (VarFree "f"), [Var' (VarParam ("a", 0))]);
   Applic' (Var' (VarFree "g"),
    [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]);
   ApplicTP' (Var' (VarFree "display"), [Const' (Sexpr (String "done!\n"))])])) ;;
let test20 = test_exp' ( (LambdaSimple ([],
 If (Applic (Var "f", [Var "x"]),
  If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
   Const (Sexpr (Bool false))),
  Const (Sexpr (Bool false))))) ) (
LambdaSimple' ([],
 If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
  If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
   ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
   Const' (Sexpr (Bool false))),
  Const' (Sexpr (Bool false))))) ;;
let test21 = test_exp' ( (Const
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
                     Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil))),
                   Nil))),
               Nil)))),
          Nil))),
      Nil)))) ) (
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
                    Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil))),
                  Nil))),
              Nil)))),
         Nil))),
     Nil)))) ;;
let test22 = test_exp' ( (LambdaSimple (["x"; "y"],
 Or [Applic (Var "f", [Applic (Var "g", [Var "x"])]); Var "y"])) ) (
LambdaSimple' (["x"; "y"],
 Or'
  [Applic' (Var' (VarFree "f"),
    [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])]);
   Var' (VarParam ("y", 1))])) ;;
let test23 = test_exp' ( (LambdaSimple (["x"], Set (Var "x", Applic (Var "f", [Var "y"])))) ) (
LambdaSimple' (["x"],
 Set'( (VarParam ("x", 0)),
  Applic' (Var' (VarFree "f"), [Var' (VarFree "y")])))) ;;
let test24 = test_exp' ( (LambdaSimple ([],
 Set (Var "x",
  Applic (Var "f",
   [LambdaSimple (["y"], Applic (Var "g", [Var "x"; Var "y"]))])))) ) (
LambdaSimple' ([],
 Set'( (VarFree "x"),
  Applic' (Var' (VarFree "f"),
   [LambdaSimple' (["y"],
     ApplicTP' (Var' (VarFree "g"),
      [Var' (VarFree "x"); Var' (VarParam ("y", 0))]))])))) ;;
let test25 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 If (Applic (Var "f?", [Var "x"]), Applic (Var "g", [Var "y"]),
  If (Applic (Var "g?", [Var "x"]),
   Seq [Applic (Var "f", [Var "x"]); Applic (Var "f", [Var "y"])],
   Seq
    [Applic (Var "h", [Var "x"]); Applic (Var "f", [Var "y"]);
     Applic (Var "g", [Applic (Var "f", [Var "x"])])])))) ) (
LambdaSimple' (["x"; "y"; "z"],
 If' (Applic' (Var' (VarFree "f?"), [Var' (VarParam ("x", 0))]),
  ApplicTP' (Var' (VarFree "g"), [Var' (VarParam ("y", 1))]),
  If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
   Seq'
    [Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))]);
     ApplicTP' (Var' (VarFree "f"), [Var' (VarParam ("y", 1))])],
   Seq'
    [Applic' (Var' (VarFree "h"), [Var' (VarParam ("x", 0))]);
     Applic' (Var' (VarFree "f"), [Var' (VarParam ("y", 1))]);
     ApplicTP' (Var' (VarFree "g"),
      [Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))])])])))) ;;
let test26 = test_exp' ( (Applic (LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"])),
 [Applic (Var "f", [Var "y"]); Applic (Var "g", [Var "x"])])) ) (
Applic'
 (LambdaSimple' (["x"; "y"],
   ApplicTP' (Var' (VarFree "+"),
    [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1))])),
 [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")]);
  Applic' (Var' (VarFree "g"), [Var' (VarFree "x")])])) ;;
let test27 = test_exp' ( (LambdaSimple (["x"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))]))) ) (
LambdaSimple' (["x"],
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
     LambdaSimple' (["y"],
      Set' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))]))) ;;
let test28 = test_exp' ( (LambdaSimple (["x"; "y"; "z"], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   Var' (VarParam ("z", 2))]))) ;;
let test29 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+", [Var "z"; Var "x"; Const (Sexpr (Number (Fraction(1,1))))]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0)); Var' (VarBound ("x", 0, 0));
      Const' (Sexpr (Number (Fraction(1,1))))]))]))) ;;
let test30 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+", [Var "z"; Const (Sexpr (Number (Fraction(2,1))))]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0)); Const' (Sexpr (Number (Fraction(2,1))))]))]))) ;;
let test31 = test_exp' ( (LambdaOpt (["x"; "y"], "z",
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z"; LambdaSimple (["z"], Applic (Var "+", [Var "z"; Var "y"]))]))]))) ) (
LambdaOpt' (["x"; "y"], "z",
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' (["z"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarParam ("z", 0)); Var' (VarBound ("y", 1, 1))]))]))]))) ;;
let test32 = test_exp' ( (Applic
  (LambdaSimple (["x"],
    If (Applic (Var "x", [Const (Sexpr (Number (Fraction(1,1))))]),
     Applic (Var "x", [Const (Sexpr (Number (Fraction(2,1))))]),
     Applic
      (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Fraction(0,1)))))),
      [Const (Sexpr (Number (Fraction(3,1))))]))),
  [LambdaSimple (["x"], Var "x")])) ) (
Applic'
 (LambdaSimple' (["x"],
   If'
    (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Fraction(1,1))))]),
    ApplicTP' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Fraction(2,1))))]),
    ApplicTP'
     (LambdaSimple' (["x"],
       Set'( (VarParam ("x", 0)), Const' (Sexpr (Number (Fraction(0,1)))))),
     [Const' (Sexpr (Number (Fraction(3,1))))]))),
 [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))])) ;;
let test33 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z";
      LambdaSimple (["x"], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' (["x"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarParam ("x", 0)); Var' (VarBound ("y", 1, 1));
         Var' (VarBound ("z", 0, 0))]))]))]))) ;;
let test34 = test_exp' ( (LambdaOpt (["x"; "y"], "z",
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z"; LambdaSimple ([], Applic (Var "+", [Var "z"; Var "y"]))]))]))) ) (
LambdaOpt' (["x"; "y"], "z",
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarBound ("z", 0, 0)); Var' (VarBound ("y", 1, 1))]))]))]))) ;;
let test35 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "x"; Var "y"; Var "z";
      LambdaSimple ([], Applic (Var "+", [Var "x"; Var "y"; Var "z"]))]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarBound ("x", 0, 0)); Var' (VarBound ("y", 0, 1));
      Var' (VarParam ("z", 0));
      LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarBound ("x", 1, 0)); Var' (VarBound ("y", 1, 1));
         Var' (VarBound ("z", 0, 0))]))]))]))) ;;
let test36 = test_exp' ( (Def (Var "foo3",
 LambdaOpt (["x"], "y",
  Seq
   [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
    LambdaSimple ([], Set (Var "x", Var "y"))]))) ) (
Def' ( (VarFree "foo3"),
 LambdaOpt' (["x"], "y",
    Seq'
     [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
      LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
      LambdaSimple' ([],
       Set' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))]))) ;;
let test37 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"],
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
      LambdaSimple' (["y"],
       Set' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])))) ;;
let test38 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"; "y"],
  Set (Var "x", Applic (Var "*", [Var "x"; Var "y"]))))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"],
  Set'( (VarParam ("x", 0)),
   Applic' (Var' (VarFree "*"),
    [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1))]))))) ;;
let test39 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"; "y"],
  If (Var "x", LambdaSimple ([], Set (Var "y", Var "x")),
   LambdaSimple (["z"], Set (Var "x", Var "z")))))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"],
    If' (Var' (VarParam ("x", 0)),
     LambdaSimple' ([],
      Set'( (VarBound ("y", 0, 1)), Var' (VarBound ("x", 0, 0)))),
     LambdaSimple' (["z"],
      Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0)))))))) ;;
let test40 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"; "y"],
  Applic (Var "list",
   [LambdaSimple ([],
     Set (Var "x",
      Applic (Var "+", [Var "x"; Const (Sexpr (Number (Fraction(1,1))))])));
    LambdaSimple ([], Var "y")])))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"],
  ApplicTP' (Var' (VarFree "list"),
   [LambdaSimple' ([],
     Set'( (VarBound ("x", 0, 0)),
      Applic' (Var' (VarFree "+"),
       [Var' (VarBound ("x", 0, 0)); Const' (Sexpr (Number (Fraction(1,1))))])));
    LambdaSimple' ([], Var' (VarBound ("y", 0, 1)))])))) ;;
let test41 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"],
  LambdaSimple (["op"],
   If (Applic (Var "eq?", [Var "op"; Const (Sexpr (Symbol "read"))]),
    LambdaSimple ([], Var "x"),
    If (Applic (Var "eq?", [Var "op"; Const (Sexpr (Symbol "write"))]),
     LambdaSimple (["val"], Set (Var "x", Var "val")), Const Void)))))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"],
  LambdaSimple' (["op"],
   If'
    (Applic' (Var' (VarFree "eq?"),
      [Var' (VarParam ("op", 0)); Const' (Sexpr (Symbol "read"))]),
    LambdaSimple' ([], Var' (VarBound ("x", 1, 0))),
    If'
     (Applic' (Var' (VarFree "eq?"),
       [Var' (VarParam ("op", 0)); Const' (Sexpr (Symbol "write"))]),
     LambdaSimple' (["val"],
      Set'( (VarBound ("x", 1, 0)), Var' (VarParam ("val", 0)))),
     Const' Void)))))) ;;
let test42 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"],
  Applic
   (LambdaSimple (["y"],
     Applic (Var "cons",
      [LambdaSimple ([], Var "x");
       Applic (Var "cons", [Set (Var "x", Var "y"); Const (Sexpr Nil)])])),
   [Const (Sexpr (Number (Fraction(1,1))))])))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"],
  ApplicTP'
   (LambdaSimple' (["y"],
     ApplicTP' (Var' (VarFree "cons"),
      [LambdaSimple' ([], Var' (VarBound ("x", 1, 0)));
       Applic' (Var' (VarFree "cons"),
        [Set'( (VarBound ("x", 0, 0)), Var' (VarParam ("y", 0)));
         Const' (Sexpr Nil)])])),
   [Const' (Sexpr (Number (Fraction(1,1))))])))) ;;
let test43 = test_exp' ( (LambdaSimple (["x"],
  Or
   [Applic
     (LambdaOpt (["y"], "z",
       Applic
        (LambdaSimple ([],
          Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
        [])),
     [Var "x"; Const (Sexpr (Number (Fraction(1,1))))]);
    LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])])) ) (
LambdaSimple' (["x"],
   Or'
    [Applic'
      (LambdaOpt' (["y"], "z",
        ApplicTP'
         (LambdaSimple' ([],
           ApplicTP'
            (LambdaSimple' ([],
              ApplicTP' (Var' (VarFree "+"),
               [Var' (VarBound ("x", 2, 0)); Var' (VarBound ("z", 1, 1))])),
            [])),
         [])),
      [Var' (VarParam ("x", 0)); Const' (Sexpr (Number (Fraction(1,1))))]);
     LambdaSimple' ([], Set' (VarBound ("x", 0, 0), Var' (VarFree "w")));
     ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])])) ;;
let test44 = test_exp' ( (Def (Var "test",
 LambdaOpt (["x"], "y",
  Applic (Var "cons", [Var "x"; LambdaSimple ([], Set (Var "x", Var "y"))])))) ) (
Def' ( (VarFree "test"),
 LambdaOpt' (["x"], "y",
    ApplicTP' (Var' (VarFree "cons"),
     [Var' (VarParam ("x", 0));
      LambdaSimple' ([],
       Set' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))])))) ;;
let test45 = test_exp' ( (Def (Var "test",
 LambdaSimple (["x"; "y"; "z"],
  Applic (Var "list",
   [LambdaSimple ([],
     Applic (Var "list",
      [LambdaSimple (["x"], Set (Var "x", Var "z"));
       LambdaSimple ([], Set (Var "x", Var "z")); Var "x"]));
    LambdaSimple (["y"], Set (Var "x", Var "y"))])))) ) (
Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"; "z"],
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' (["x"],
          Set'( (VarParam ("x", 0)), Var' (VarBound ("z", 1, 2))));
         LambdaSimple' ([],
          Set' (VarBound ("x", 1, 0), Var' (VarBound ("z", 1, 2))));
         Var' (VarBound ("x", 0, 0))]));
      LambdaSimple' (["y"],
       Set' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])))) ;;
let test46 = test_exp' ( (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) ) (
LambdaSimple' (["x"; "y"],
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
     LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
     LambdaSimple' (["z"],
      Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))]))) ;;
let test47 = test_exp' ( (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "y", Var "z"));
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) ) (
LambdaSimple' (["x"; "y"],
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
     LambdaSimple' (["z"],
      Set'( (VarBound ("y", 0, 1)), Var' (VarParam ("z", 0))));
     LambdaSimple' (["z"],
      Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))]))) ;;
let test48 = test_exp' ( (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
   LambdaSimple (["z"], Set (Var "y", Var "z"));
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) ) (
LambdaSimple' (["x"; "y"],
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
     LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
     LambdaSimple' (["z"],
      Set' (VarBound ("y", 0, 1), Var' (VarParam ("z", 0))));
     LambdaSimple' (["z"],
      Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))]))) ;;
let test49 = test_exp' ( (Def (Var "func",
 LambdaOpt ([], "x",
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "x", Var "z"));
    LambdaSimple (["z"], Set (Var "x", Var "z"))])))) ) (
Def' ( (VarFree "func"),
 LambdaOpt' ([], "x",
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
      LambdaSimple' (["z"],
       Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))));
      LambdaSimple' (["z"],
       Set' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])))) ;;
let test50 = test_exp' ( (Def (Var "func",
 LambdaOpt ([], "x",
  LambdaSimple (["samerib"],
   Applic (Var "list",
    [LambdaSimple ([], Var "x");
     LambdaSimple (["z"], Set (Var "x", Var "z"))]))))) ) (
Def' ( (VarFree "func"),
 LambdaOpt' ([], "x",
  LambdaSimple' (["samerib"],
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], Var' (VarBound ("x", 1, 0)));
     LambdaSimple' (["z"],
      Set'( (VarBound ("x", 1, 0)), Var' (VarParam ("z", 0))))]))))) ;;
let test51 = test_exp' ( (Def (Var "func",
 LambdaSimple (["x"; "y"; "z"; "w"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
    LambdaSimple ([], Var "z"); LambdaSimple ([], Var "w");
    LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction(0,1))))));
    LambdaSimple ([], Set (Var "y", Const (Sexpr (Number (Fraction(1,1))))));
    LambdaSimple ([], Set (Var "z", Const (Sexpr (Number (Fraction(2,1))))));
    LambdaSimple ([], Set (Var "w", Const (Sexpr (Number (Fraction(3,1))))))])))) ) (
Def' ( (VarFree "func"),
 LambdaSimple' (["x"; "y"; "z"; "w"],
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
      LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
      LambdaSimple' ([], Var' (VarBound ("z", 0, 2)));
      LambdaSimple' ([], Var' (VarBound ("w", 0, 3)));
      LambdaSimple' ([],
       Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(0,1))))));
      LambdaSimple' ([],
       Set' (VarBound ("y", 0, 1), Const' (Sexpr (Number (Fraction(1,1))))));
      LambdaSimple' ([],
       Set' (VarBound ("z", 0, 2), Const' (Sexpr (Number (Fraction(2,1))))));
      LambdaSimple' ([],
       Set' (VarBound ("w", 0, 3), Const' (Sexpr (Number (Fraction(3,1))))))])))) ;;
let test52 = test_exp' ( (Def (Var "x",
 Applic (Var "+",
  [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))))]))) ) (
Def' ( (VarFree "x"),
 Applic' (Var' (VarFree "+"),
  [Const' (Sexpr (Number (Fraction(1,1)))); Const' (Sexpr (Number (Fraction(2,1))))]))) ;;
let test53 = test_exp' ( (LambdaOpt (["x"], "y", Var "x")) ) ( LambdaOpt' (["x"], "y", Var' (VarParam ("x", 0)))) ;;
let test54 = test_exp' ( (If (Applic (LambdaSimple (["y"], Var "x"), []),
  Applic
   (LambdaSimple (["x"],
     Seq
      [Set (Var "x", Var "y");
       LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction(1,1))))))]),
   [Const (Sexpr (Symbol "a"))]),
  LambdaSimple (["x"], Set (Var "x", Var "y")))) ) (
If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []),
 Applic'
  (LambdaSimple' (["x"],
    Seq'
     [Set'( (VarParam ("x", 0)), Var' (VarFree "y"));
      LambdaSimple' ([],
       Set'( (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Fraction(1,1))))))]),
  [Const' (Sexpr (Symbol "a"))]),
 LambdaSimple' (["x"], Set'( (VarParam ("x", 0)), Var' (VarFree "y"))))) ;;
let test55 = test_exp' ( (Applic
 (LambdaSimple (["x"],
   Applic (LambdaSimple (["y"], Applic (Var "+", [Var "x"; Var "y"])),
    [Applic (Var "g", [Var "x"])])),
 [Applic (Var "f", [Var "y"])])) ) (
Applic'
 (LambdaSimple' (["x"],
   ApplicTP'
    (LambdaSimple' (["y"],
      ApplicTP' (Var' (VarFree "+"),
       [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])),
    [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])])),
 [Applic' (Var' (VarFree "f"), [Var' (VarFree "y")])])) ;;
let test56 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "w"],
  If (Applic (Var "foo?", [Var "x"]), Applic (Var "goo", [Var "y"]),
   Applic (Var "boo", [Applic (Var "doo", [Var "z"])])))) ) (
LambdaSimple' (["x"; "y"; "z"; "w"],
 If' (Applic' (Var' (VarFree "foo?"), [Var' (VarParam ("x", 0))]),
  ApplicTP' (Var' (VarFree "goo"), [Var' (VarParam ("y", 1))]),
  ApplicTP' (Var' (VarFree "boo"),
   [Applic' (Var' (VarFree "doo"), [Var' (VarParam ("z", 2))])])))) ;;
let test57 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
  Applic (Var "f",
   [If (Applic (Var "g?", [Var "x"]), Applic (Var "h", [Var "y"]),
     Applic (Var "w", [Var "z"]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP' (Var' (VarFree "f"),
  [If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
    Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
    Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))]))) ;;
let test58 = test_exp' ( (LambdaSimple ([],
  If (Applic (Var "f", [Var "x"]),
   If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
    Const (Sexpr (Bool false))),
   Const (Sexpr (Bool false))))) ) (
LambdaSimple' ([],
 If' (Applic' (Var' (VarFree "f"), [Var' (VarFree "x")]),
  If' (Applic' (Var' (VarFree "g"), [Var' (VarFree "y")]),
   ApplicTP' (Var' (VarFree "h"), [Var' (VarFree "z")]),
   Const' (Sexpr (Bool false))),
  Const' (Sexpr (Bool false))))) ;;
let test59 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "a"; "b"],
  Applic (Var "f",
   [If (Applic (Var "g?", [Var "x"]),
     If (Applic (Var ">", [Const (Sexpr (Number (Fraction(1,1)))); Var "a"]),
      Applic (Var "h", [Var "y"]), Var "b"),
     Applic (Var "w", [Var "z"]))]))) ) (
LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
 ApplicTP' (Var' (VarFree "f"),
  [If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
    If'
     (Applic' (Var' (VarFree ">"),
       [Const' (Sexpr (Number (Fraction(1,1)))); Var' (VarParam ("a", 3))]),
     Applic' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
     Var' (VarParam ("b", 4))),
    Applic' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))]))) ;;
let test60 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "a"; "b"],
  If (Applic (Var "g?", [Var "x"]),
   If (Applic (Var ">", [Const (Sexpr (Number (Fraction(1,1)))); Var "a"]),
    Applic (Var "h", [Var "y"]), Applic (Var "w", [Var "b"])),
   If (Applic (Var "<", [Const (Sexpr (Number (Fraction(1,1)))); Var "a"]),
    Applic (Var "w", [Var "b"]), Applic (Var "w", [Var "z"]))))) ) (
LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
 If' (Applic' (Var' (VarFree "g?"), [Var' (VarParam ("x", 0))]),
  If'
   (Applic' (Var' (VarFree ">"),
     [Const' (Sexpr (Number (Fraction(1,1)))); Var' (VarParam ("a", 3))]),
   ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("y", 1))]),
   ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("b", 4))])),
  If'
   (Applic' (Var' (VarFree "<"),
     [Const' (Sexpr (Number (Fraction(1,1)))); Var' (VarParam ("a", 3))]),
   ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("b", 4))]),
   ApplicTP' (Var' (VarFree "w"), [Var' (VarParam ("z", 2))]))))) ;;
let test61 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "a"; "b"],
  If (Or [Applic (Var "f", [Applic (Var "g", [Var "x"])]); Var "y"],
   If (Applic (Var "f", [Var "x"]),
    If (Applic (Var "g", [Var "y"]), Applic (Var "h", [Var "z"]),
     Const (Sexpr (Bool false))),
    Const (Sexpr (Bool false))),
   Applic (Var "h", [Var "z"])))) ) (
LambdaSimple' (["x"; "y"; "z"; "a"; "b"],
 If'
  (Or'
    [Applic' (Var' (VarFree "f"),
      [Applic' (Var' (VarFree "g"), [Var' (VarParam ("x", 0))])]);
     Var' (VarParam ("y", 1))],
  If' (Applic' (Var' (VarFree "f"), [Var' (VarParam ("x", 0))]),
   If' (Applic' (Var' (VarFree "g"), [Var' (VarParam ("y", 1))]),
    ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("z", 2))]),
    Const' (Sexpr (Bool false))),
   Const' (Sexpr (Bool false))),
  ApplicTP' (Var' (VarFree "h"), [Var' (VarParam ("z", 2))])))) ;;
let test62 = test_exp' ( (LambdaSimple (["s"], Applic (Var "apply", [Var "f"; Var "s"]))) ) (
LambdaSimple' (["s"],
 ApplicTP' (Var' (VarFree "apply"),
  [Var' (VarFree "f"); Var' (VarParam ("s", 0))]))) ;;
let test63 = test_exp' ( (Applic
  (LambdaSimple (["a"; "b"; "c"; "e"],
    If (Applic (Var "eq?", [Var "a"; Const (Sexpr (Number (Fraction(5,1))))]),
     Applic (Var "e", [Var "a"; Var "b"]),
     Applic (Var "c", [Var "a"; Var "b"]))),
  [Const (Sexpr (Number (Fraction(5,1)))); Const (Sexpr (Number (Fraction(7,1)))); Var "d";
   Var "f"])) ) (
Applic'
 (LambdaSimple' (["a"; "b"; "c"; "e"],
   If'
    (Applic' (Var' (VarFree "eq?"),
      [Var' (VarParam ("a", 0)); Const' (Sexpr (Number (Fraction(5,1))))]),
    ApplicTP' (Var' (VarParam ("e", 3)),
     [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]),
    ApplicTP' (Var' (VarParam ("c", 2)),
     [Var' (VarParam ("a", 0)); Var' (VarParam ("b", 1))]))),
 [Const' (Sexpr (Number (Fraction(5,1)))); Const' (Sexpr (Number (Fraction(7,1))));
  Var' (VarFree "d"); Var' (VarFree "f")])) ;;
let test64 = test_exp' ( (Applic
  (LambdaSimple (["a"],
    Applic
     (LambdaSimple (["b"],
       Applic
        (LambdaSimple (["c"],
          Applic
           (LambdaSimple (["e"],
             If
              (Applic (Var "eq?", [Var "a"; Const (Sexpr (Number (Fraction(5,1))))]),
              Applic (Var "e", [Var "a"; Var "b"]),
              Applic (Var "c", [Var "a"; Var "b"]))),
           [Var "f"])),
        [Var "d"])),
     [Const (Sexpr (Number (Fraction(7,1))))])),
  [Const (Sexpr (Number (Fraction(5,1))))])) ) (
Applic'
 (LambdaSimple' (["a"],
   ApplicTP'
    (LambdaSimple' (["b"],
      ApplicTP'
       (LambdaSimple' (["c"],
         ApplicTP'
          (LambdaSimple' (["e"],
            If'
             (Applic' (Var' (VarFree "eq?"),
               [Var' (VarBound ("a", 2, 0)); Const' (Sexpr (Number (Fraction(5,1))))]),
             ApplicTP' (Var' (VarParam ("e", 0)),
              [Var' (VarBound ("a", 2, 0)); Var' (VarBound ("b", 1, 0))]),
             ApplicTP' (Var' (VarBound ("c", 0, 0)),
              [Var' (VarBound ("a", 2, 0)); Var' (VarBound ("b", 1, 0))]))),
          [Var' (VarFree "f")])),
       [Var' (VarFree "d")])),
    [Const' (Sexpr (Number (Fraction(7,1))))])),
 [Const' (Sexpr (Number (Fraction(5,1))))])) ;;
let test65 = test_exp' ( (LambdaOpt (["x"; "y"; "z"], "w",
  Seq
   [Var "z";
    Applic
     (LambdaSimple ([],
       Seq [Set (Var "w", Var "w"); Applic (Applic (Var "y", [Var "x"]), [])]),
     [])])) ) (
LambdaOpt' (["x"; "y"; "z"], "w",
 Seq'
  [Var' (VarParam ("z", 2));
   ApplicTP'
    (LambdaSimple' ([],
      Seq'
       [Set'( (VarBound ("w", 0, 3)), Var' (VarBound ("w", 0, 3)));
        ApplicTP'
         (Applic' (Var' (VarBound ("y", 0, 1)),
           [Var' (VarBound ("x", 0, 0))]),
         [])]),
    [])])) ;;
let test66 = test_exp' ( (Def (Var "while",
  LambdaSimple (["test"; "body"],
   If (Applic (Var "test", []),
    Seq
     [Applic (Var "body", []);
      Applic (Var "while", [Var "test"; Var "body"])],
    Const Void)))) ) (
Def' ( (VarFree "while"),
 LambdaSimple' (["test"; "body"],
  If' (Applic' (Var' (VarParam ("test", 0)), []),
   Seq'
    [Applic' (Var' (VarParam ("body", 1)), []);
     ApplicTP' (Var' (VarFree "while"),
      [Var' (VarParam ("test", 0)); Var' (VarParam ("body", 1))])],
   Const' Void)))) ;;
let test67 = test_exp' ( (Applic
  (LambdaSimple (["a"],
    Applic
     (LambdaSimple (["c"],
       Applic
        (LambdaSimple (["e"],
          If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction(5,1))))]),
           LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
           LambdaSimple (["x"], Applic (Var "c", [Var "x"])))),
        [Var "f"])),
     [Var "d"])),
  [Var "b"])) ) (
Applic'
 (LambdaSimple' (["a"],
   ApplicTP'
    (LambdaSimple' (["c"],
      ApplicTP'
       (LambdaSimple' (["e"],
         If'
          (Applic' (Var' (VarFree "x"),
            [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction(5,1))))]),
          LambdaSimple' (["x"],
           ApplicTP' (Var' (VarBound ("a", 2, 0)),
            [Var' (VarParam ("x", 0))])),
          LambdaSimple' (["x"],
           ApplicTP' (Var' (VarBound ("c", 1, 0)),
            [Var' (VarParam ("x", 0))])))),
       [Var' (VarFree "f")])),
    [Var' (VarFree "d")])),
 [Var' (VarFree "b")])) ;;
let test68 = test_exp' ( (LambdaSimple (["x"],
  LambdaOpt (["x"], "y",
   If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction(5,1))))]),
    LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
    LambdaSimple (["x"], Applic (Var "c", [Var "x"])))))) ) (
LambdaSimple' (["x"],
 LambdaOpt' (["x"], "y",
  If'
   (Applic' (Var' (VarParam ("x", 0)),
     [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction(5,1))))]),
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("x", 0))])),
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarFree "c"), [Var' (VarParam ("x", 0))])))))) ;;
let test69 = test_exp' ( (LambdaSimple (["x"],
  LambdaOpt (["a"], "y",
   If (Applic (Var "x", [Var ">"; Const (Sexpr (Number (Fraction(5,1))))]),
    LambdaSimple (["x"], Applic (Var "a", [Var "x"])),
    LambdaSimple (["x"], Applic (Var "c", [Var "x"])))))) ) (
LambdaSimple' (["x"],
 LambdaOpt' (["a"], "y",
  If'
   (Applic' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarFree ">"); Const' (Sexpr (Number (Fraction(5,1))))]),
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarBound ("a", 0, 0)), [Var' (VarParam ("x", 0))])),
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarFree "c"), [Var' (VarParam ("x", 0))])))))) ;;
let test70 = test_exp' ( (LambdaSimple (["a"],
  Seq
   [LambdaSimple ([],
     LambdaSimple (["x"; "y"; "z"],
      Or [Applic (Var "x", [Var "y"]); Applic (Var "a", [Var "z"])]));
    LambdaSimple (["x"], Applic (Var "a", [Var "x"]))])) ) (
LambdaSimple' (["a"],
 Seq'
  [LambdaSimple' ([],
    LambdaSimple' (["x"; "y"; "z"],
     Or'
      [Applic' (Var' (VarParam ("x", 0)), [Var' (VarParam ("y", 1))]);
       ApplicTP' (Var' (VarBound ("a", 1, 0)), [Var' (VarParam ("z", 2))])]));
   LambdaSimple' (["x"],
    ApplicTP' (Var' (VarBound ("a", 0, 0)), [Var' (VarParam ("x", 0))]))])) ;;
let test71 = test_exp' ( (Seq
  [LambdaSimple (["x"; "y"],
    LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])));
   LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))]) ) (
Seq'
 [LambdaSimple' (["x"; "y"],
   LambdaSimple' (["y"; "z"],
    ApplicTP' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])));
  LambdaSimple' (["z"],
   ApplicTP' (Var' (VarFree "x"),
    [Var' (VarFree "y"); Var' (VarParam ("z", 0))]))]) ;;
let test72 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
  Applic
   (If (Applic (Var "x", [Var "y"; Var "z"]),
     Seq
      [LambdaSimple (["x"; "y"],
        LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])));
       LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))],
     Const Void),
   [LambdaSimple ([], Applic (Var "x", [Var "y"; Var "z"]))]))) ) (
LambdaSimple' (["x"; "y"; "z"],
 ApplicTP'
  (If'
    (Applic' (Var' (VarParam ("x", 0)),
      [Var' (VarParam ("y", 1)); Var' (VarParam ("z", 2))]),
    Seq'
     [LambdaSimple' (["x"; "y"],
       LambdaSimple' (["y"; "z"],
        ApplicTP' (Var' (VarBound ("x", 0, 0)),
         [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])));
      LambdaSimple' (["z"],
       ApplicTP' (Var' (VarBound ("x", 0, 0)),
        [Var' (VarBound ("y", 0, 1)); Var' (VarParam ("z", 0))]))],
    Const' Void),
  [LambdaSimple' ([],
    ApplicTP' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarBound ("y", 0, 1)); Var' (VarBound ("z", 0, 2))]))]))) ;;
let test73 = test_exp' ( (LambdaSimple (["x"],
  Applic (Var "x",
   [LambdaSimple (["y"; "z"],
     Applic (Var "x",
      [Var "y"; Var "z";
       LambdaSimple ([],
        Applic (Var "x",
         [Var "y"; Var "z";
          LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))]))]))]))) ) (
LambdaSimple' (["x"],
 ApplicTP' (Var' (VarParam ("x", 0)),
  [LambdaSimple' (["y"; "z"],
    ApplicTP' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1));
      LambdaSimple' ([],
       ApplicTP' (Var' (VarBound ("x", 1, 0)),
        [Var' (VarBound ("y", 0, 0)); Var' (VarBound ("z", 0, 1));
         LambdaSimple' (["z"],
          ApplicTP' (Var' (VarBound ("x", 2, 0)),
           [Var' (VarBound ("y", 1, 0)); Var' (VarParam ("z", 0))]))]))]))]))) ;;
let test74 = test_exp' ( (LambdaSimple (["x"],
  Applic (Var "x",
   [LambdaSimple (["y"; "z"],
     Applic (Var "x",
      [Var "y"; Var "z";
       LambdaSimple ([],
        Applic (Var "x",
         [Var "y"; Var "z";
          LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))]))]));
    LambdaSimple (["x"],
     LambdaSimple (["y"; "z"], Applic (Var "x", [Var "y"; Var "z"])))]))) ) (
LambdaSimple' (["x"],
 ApplicTP' (Var' (VarParam ("x", 0)),
  [LambdaSimple' (["y"; "z"],
    ApplicTP' (Var' (VarBound ("x", 0, 0)),
     [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1));
      LambdaSimple' ([],
       ApplicTP' (Var' (VarBound ("x", 1, 0)),
        [Var' (VarBound ("y", 0, 0)); Var' (VarBound ("z", 0, 1));
         LambdaSimple' (["z"],
          ApplicTP' (Var' (VarBound ("x", 2, 0)),
           [Var' (VarBound ("y", 1, 0)); Var' (VarParam ("z", 0))]))]))]));
   LambdaSimple' (["x"],
    LambdaSimple' (["y"; "z"],
     ApplicTP' (Var' (VarBound ("x", 0, 0)),
      [Var' (VarParam ("y", 0)); Var' (VarParam ("z", 1))])))]))) ;;
let test75 = test_exp' ( (LambdaSimple (["x"; "y"; "z"],
  Seq
   [Applic (Var "x",
     [LambdaSimple (["x"],
       LambdaSimple (["y"],
        LambdaSimple (["z"], Applic (Var "x", [Var "y"; Var "z"]))))]);
    LambdaSimple ([], Applic (Var "z", []))])) ) (
LambdaSimple' (["x"; "y"; "z"],
 Seq'
  [Applic' (Var' (VarParam ("x", 0)),
    [LambdaSimple' (["x"],
      LambdaSimple' (["y"],
       LambdaSimple' (["z"],
        ApplicTP' (Var' (VarBound ("x", 1, 0)),
         [Var' (VarBound ("y", 0, 0)); Var' (VarParam ("z", 0))]))))]);
   LambdaSimple' ([], ApplicTP' (Var' (VarBound ("z", 0, 2)), []))])) ;;
let test76 = test_exp' ( (Def (Var "a",
  Applic
   (LambdaSimple ([],
     LambdaOpt ([], "x",
      Seq
       [Var "x";
        LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Fraction(1,1))))))])),
   []))) ) (
Def' ( (VarFree "a"),
 Applic'
  (LambdaSimple' ([],
    LambdaOpt' ([], "x",
     Seq'
      [Var' (VarParam ("x", 0));
       LambdaOpt' ([], "y",
        Set'( (VarParam ("y", 0)), Const' (Sexpr (Number (Fraction(1,1))))))])),
  []))) ;;
let test77 = test_exp' ( (LambdaSimple (["x"],
  Applic (Var "eq?",
   [LambdaSimple (["x"], Applic (Var "x", []));
    LambdaSimple ([], LambdaSimple (["x"], Applic (Var "x", [])))]))) ) (
LambdaSimple' (["x"],
 ApplicTP' (Var' (VarFree "eq?"),
  [LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), []));
   LambdaSimple' ([],
    LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), [])))]))) ;;
let test78 = test_exp' ( (LambdaSimple (["x"],
  LambdaSimple ([],
   Seq
    [Applic (LambdaSimple (["x"], Applic (Var "x", [])), [Var "x"]);
     LambdaSimple ([], LambdaSimple ([], Applic (Var "x", [])))]))) ) (
LambdaSimple' (["x"],
 LambdaSimple' ([],
  Seq'
   [Applic' (LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), [])),
     [Var' (VarBound ("x", 0, 0))]);
    LambdaSimple' ([],
     LambdaSimple' ([], ApplicTP' (Var' (VarBound ("x", 2, 0)), [])))]))) ;;
let test79 = test_exp' ( (LambdaSimple (["x"; "y"; "z"; "w"],
  Applic (Var "w",
   [LambdaSimple (["x"; "y"; "z"],
     Seq
      [Applic (Var "z",
        [LambdaSimple (["x"; "y"],
          Applic (Var "y", [LambdaSimple (["x"], Applic (Var "x", []))]));
         Var "x"]);
       Var "x"]);
    Var "x"; Var "y"; Var "z"]))) ) (
LambdaSimple' (["x"; "y"; "z"; "w"],
 ApplicTP' (Var' (VarParam ("w", 3)),
  [LambdaSimple' (["x"; "y"; "z"],
    Seq'
     [Applic' (Var' (VarParam ("z", 2)),
       [LambdaSimple' (["x"; "y"],
         ApplicTP' (Var' (VarParam ("y", 1)),
          [LambdaSimple' (["x"], ApplicTP' (Var' (VarParam ("x", 0)), []))]));
        Var' (VarParam ("x", 0))]);
      Var' (VarParam ("x", 0))]);
   Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   Var' (VarParam ("z", 2))]))) ;;
let test80 = test_exp' ( (LambdaSimple (["x"; "y"],
  Seq
   [Applic (Var "x", [Var "y"]);
    LambdaSimple ([],
     LambdaSimple ([],
      LambdaSimple ([],
       Set (Var "x",
        Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))])) ) (
LambdaSimple' (["x"; "y"],
   Seq'
    [Applic' (Var' (VarParam ("x", 0)), [Var' (VarParam ("y", 1))]);
     LambdaSimple' ([],
      LambdaSimple' ([],
       LambdaSimple' ([],
        Set' (VarBound ("x", 2, 0),
         Applic'
          (LambdaSimple' (["z"],
            Set' (VarBound ("y", 3, 1), Var' (VarBound ("x", 3, 0)))),
          [Var' (VarBound ("y", 2, 1))])))))])) ;;
let test81 = test_exp' ( (LambdaSimple ([],
  Seq
   [Applic (LambdaSimple ([], Var "x"), []);
    Applic
     (LambdaSimple (["x"],
       Seq
        [Set (Var "x", Const (Sexpr (Number (Fraction(1,1)))));
         LambdaSimple ([], Var "x")]),
     [Const (Sexpr (Number (Fraction(2,1))))]);
    Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Fraction(3,1))))])])) ) (
LambdaSimple' ([],
 Seq'
  [Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);
   Applic'
    (LambdaSimple' (["x"],
        Seq'
         [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
          LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]),
    [Const' (Sexpr (Number (Fraction(2,1))))]);
   ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))),
    [Const' (Sexpr (Number (Fraction(3,1))))])])) ;;