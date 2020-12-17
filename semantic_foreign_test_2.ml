
#use "semantic-analyser.ml";;
open Semantics;;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
  | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
  | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
  | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
  | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
   (expr'_eq e1 e2) &&
     (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;	


let test e1 e2 = try (expr'_eq (run_semantics e1) e2)
  with exp -> Printf.printf "(failed with exception) "; false;;


let test_0 = (test (LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Fraction(5,1)))));
       Applic (Var "+", [Var "x"; Var "y"])]);
    Applic (Var "+", [Var "x"; Var "y"; Var "z"])])
  ) (
  LambdaSimple' (["x"; "y"; "z"],
   Seq'
  [Set' (VarParam ("x", 0), Box' (VarParam ("x", 0)));
    LambdaSimple' (["y"],
      Seq'
       [BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(5,1)))));
        ApplicTP' (Var' (VarFree "+"),
         [BoxGet' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])]);
     ApplicTP' (Var' (VarFree "+"),
      [BoxGet' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       Var' (VarParam ("z", 2))])])
  ));;

let test_1 = (test (LambdaSimple (["x"], Set (Var "x", Applic (LambdaSimple ([], Var "x"), [])))
  ) (
  LambdaSimple' (["x"],
 Seq'
  [Set'  (VarParam ("x", 0), Box' (VarParam ("x", 0)));
   BoxSet' (VarParam ("x", 0),
    Applic' (LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0))), []))])));;


let test_2 = (test (Applic (Var "y",
  [LambdaSimple (["y"],
    Seq
     [Set (Var "a", LambdaSimple (["b"], Applic (Var "a", [Var "b"])));
      Set (Var "t",
       LambdaSimple (["x"],
        Seq
         [Set (Var "y",
           LambdaSimple (["j"], Applic (Var "x", [Var "j"; Var "x"])));
          Var "h"]));
      Applic (Var "y", [Var "a"])])])) (
  Applic' (Var' (VarFree "y"),
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
       ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])])
  ));;

let test_3 = (test (LambdaSimple (["x"; "y"],
  Seq
   [LambdaSimple ([], Set (Var "x", Var "y"));
    LambdaSimple ([], Set (Var "y", Var "x"))])
  ) (
  LambdaSimple' (["x"; "y"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   Set' ( (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
    LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("y", 0, 1))));
     LambdaSimple' ([],
      BoxSet' (VarBound ("y", 0, 1), BoxGet' (VarBound ("x", 0, 0))))])
  ));;

let test_4 = (test (LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Fraction(1,1)))));
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])
  ) (
  LambdaSimple' (["x"],
  Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    BoxGet' (VarParam ("x", 0));
     LambdaSimple' (["x"],
      Seq'
       [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
         BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
          LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))]);
     LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])
  ));;

let test_5 = (test (LambdaSimple (["x"],
  Seq
   [Var "x";
    LambdaSimple (["x"],
     Seq
      [Set (Var "y", Var "x");
       LambdaSimple ([], Var "x")]);
    LambdaSimple ([], Set (Var "x", Var "x"))])
  ) (
  LambdaSimple' (["x"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   
    BoxGet' (VarParam ("x", 0));
     LambdaSimple' (["x"],
      Seq'
       [Set' ( (VarFree "y"), Var' (VarParam ("x", 0)));
        LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]);
     LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])
  ));;


let test_6 = (test (LambdaSimple (["x"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))]))) (
  LambdaSimple' (["x"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
     LambdaSimple' (["y"],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])])
  ));;


let test_7 = (test (LambdaSimple (["x"],
  Or
   [Applic
     (LambdaOpt (["y"], "z",
       Applic
        (LambdaSimple ([],
          Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
        [])),
     [Var "x"; Const (Sexpr (Number (Fraction(1,1))))]);
    LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])])
  ) (
  LambdaSimple' (["x"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   Or'
    [Applic'
      (LambdaOpt' (["y"], "z",
        ApplicTP'
         (LambdaSimple' ([],
           ApplicTP'
            (LambdaSimple' ([],
              ApplicTP' (Var' (VarFree "+"),
               [BoxGet' (VarBound ("x", 2, 0)); Var' (VarBound ("z", 1, 1))])),
            [])),
         [])),
      [BoxGet' (VarParam ("x", 0)); Const' (Sexpr (Number (Fraction(1,1))))]);
     LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "w")));
     ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])]])
  ));;



  let test_8 = (test (Def (Var "foo3",
  LambdaOpt (["x"], "y",
  Seq
   [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
    LambdaSimple ([], Set (Var "x", Var "y"))]))) (
  Def' ( (VarFree "foo3"),
 LambdaOpt' (["x"], "y",
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
     LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
      LambdaSimple' ([],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))]))
  ));;

let test_9 = (test (Def (Var "test",
 LambdaSimple (["x"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])))) (
  Def' ( (VarFree "test"),
 LambdaSimple' (["x"],
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' (["y"],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])]))
  ));;

let test_10 = (test (Def (Var "test",
 LambdaSimple (["x"; "y"],
  If (Var "x", LambdaSimple ([], Set (Var "y", Var "x")),
   LambdaSimple (["z"], Set (Var "x", Var "z")))))) (
  Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"],
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    If' (BoxGet' (VarParam ("x", 0)),
     LambdaSimple' ([],
      Set' ( (VarBound ("y", 0, 1)), BoxGet' (VarBound ("x", 0, 0)))),
     LambdaSimple' (["z"],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0)))))]))
  ));;


let test_11 = (test (Def (Var "test",
 LambdaOpt (["x"], "y",
  Applic (Var "cons", [Var "x"; LambdaSimple ([], Set (Var "x", Var "y"))])))) (
  Def' ( (VarFree "test"),
 LambdaOpt' (["x"], "y",
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "cons"),
     [BoxGet' (VarParam ("x", 0));
      LambdaSimple' ([],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarBound ("y", 0, 1))))])]))
  ));;




let test_12 = (test (Def (Var "test",
 LambdaSimple (["x"; "y"; "z"],
  Applic (Var "list",
   [LambdaSimple ([],
     Applic (Var "list",
      [LambdaSimple (["x"], Set (Var "x", Var "z"));
       LambdaSimple ([], Set (Var "x", Var "z")); Var "x"]));
    LambdaSimple (["y"], Set (Var "x", Var "y"))])))) (
  Def' ( (VarFree "test"),
 LambdaSimple' (["x"; "y"; "z"],
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([],
       ApplicTP' (Var' (VarFree "list"),
        [LambdaSimple' (["x"],
          Set' ( (VarParam ("x", 0)), Var' (VarBound ("z", 1, 2))));
         LambdaSimple' ([],
          BoxSet' (VarBound ("x", 1, 0), Var' (VarBound ("z", 1, 2))));
         BoxGet' (VarBound ("x", 0, 0))]));
      LambdaSimple' (["y"],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])]))));;

let test_13 = (test (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) (
  LambdaSimple' (["x"; "y"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
     LambdaSimple' ([], Var' (VarBound ("y", 0, 1)));
     LambdaSimple' (["z"],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])])
  ));;
let test_14 = (test (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "y", Var "z"));
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) (
  LambdaSimple' (["x"; "y"],
  Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
     LambdaSimple' (["z"],
      Set' ( (VarBound ("y", 0, 1)), Var' (VarParam ("z", 0))));
     LambdaSimple' (["z"],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])])
  ));;
let test_15 = (test (LambdaSimple (["x"; "y"],
 Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
   LambdaSimple (["z"], Set (Var "y", Var "z"));
   LambdaSimple (["z"], Set (Var "x", Var "z"))]))) (
  LambdaSimple' (["x"; "y"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   Set' ( (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
   ApplicTP' (Var' (VarFree "list"),
    [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
     LambdaSimple' ([], BoxGet' (VarBound ("y", 0, 1)));
     LambdaSimple' (["z"],
      BoxSet' (VarBound ("y", 0, 1), Var' (VarParam ("z", 0))));
     LambdaSimple' (["z"],
      BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])])
  ));;
let test_16 = (test (Def (Var "func",
 LambdaOpt ([], "x",
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple (["z"], Set (Var "x", Var "z"));
    LambdaSimple (["z"], Set (Var "x", Var "z"))])))) (
  Def' ( (VarFree "func"),
 LambdaOpt' ([], "x",
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' (["z"],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))));
      LambdaSimple' (["z"],
       BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("z", 0))))])]))
  ));;
let test_17 = (test (Def (Var "func",
 LambdaSimple (["x"; "y"; "z"; "w"],
  Applic (Var "list",
   [LambdaSimple ([], Var "x"); LambdaSimple ([], Var "y");
    LambdaSimple ([], Var "z"); LambdaSimple ([], Var "w");
    LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Fraction(0,1))))));
    LambdaSimple ([], Set (Var "y", Const (Sexpr (Number (Fraction(1,1))))));
    LambdaSimple ([], Set (Var "z", Const (Sexpr (Number (Fraction(2,1))))));
    LambdaSimple ([], Set (Var "w", Const (Sexpr (Number (Fraction(3,1))))))])))) (
  Def' ( (VarFree "func"),
 LambdaSimple' (["x"; "y"; "z"; "w"],
  Seq'
   [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    Set' ( (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
    Set' ( (VarParam ("z", 2)), Box' (VarParam ("z", 2)));
    Set' ( (VarParam ("w", 3)), Box' (VarParam ("w", 3)));
    ApplicTP' (Var' (VarFree "list"),
     [LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
      LambdaSimple' ([], BoxGet' (VarBound ("y", 0, 1)));
      LambdaSimple' ([], BoxGet' (VarBound ("z", 0, 2)));
      LambdaSimple' ([], BoxGet' (VarBound ("w", 0, 3)));
      LambdaSimple' ([],
       BoxSet' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(0,1))))));
      LambdaSimple' ([],
       BoxSet' (VarBound ("y", 0, 1), Const' (Sexpr (Number (Fraction(1,1))))));
      LambdaSimple' ([],
       BoxSet' (VarBound ("z", 0, 2), Const' (Sexpr (Number (Fraction(2,1))))));
      LambdaSimple' ([],
       BoxSet' (VarBound ("w", 0, 3), Const' (Sexpr (Number (Fraction(3,1))))))])]))
  ));;
let test_18 = (test (LambdaSimple (["x"; "y"],
  Seq
   [Applic (Var "x", [Var "y"]);
    LambdaSimple ([],
     LambdaSimple ([],
      LambdaSimple ([],
       Set (Var "x",
        Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))])
  ) (
  LambdaSimple' (["x"; "y"],
 Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
   Set' ( (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
    Applic' (BoxGet' (VarParam ("x", 0)), [BoxGet' (VarParam ("y", 1))]);
     LambdaSimple' ([],
      LambdaSimple' ([],
       LambdaSimple' ([],
        BoxSet' (VarBound ("x", 2, 0),
         Applic'
          (LambdaSimple' (["z"],
            BoxSet' (VarBound ("y", 3, 1), BoxGet' (VarBound ("x", 3, 0)))),
          [BoxGet' (VarBound ("y", 2, 1))])))))])
  ));;
let test_19 = (test (LambdaSimple ([],
  Seq
   [Applic (LambdaSimple ([], Var "x"), []);
    Applic
     (LambdaSimple (["x"],
       Seq
        [Set (Var "x", Const (Sexpr (Number (Fraction(1,1)))));
         LambdaSimple ([], Var "x")]),
     [Const (Sexpr (Number (Fraction(2,1))))]);
    Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Fraction(3,1))))])])
  ) (
  LambdaSimple' ([],
 Seq'
  [Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);
   Applic'
    (LambdaSimple' (["x"],
      Seq'
       [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
        
         BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
          LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))]),
    [Const' (Sexpr (Number (Fraction(2,1))))]);
   ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))),
    [Const' (Sexpr (Number (Fraction(3,1))))])])
  ));;
