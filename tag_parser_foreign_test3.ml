#use "tag-parser.ml";;
let exp_list_equal my_exp_list_1 my_exp_list_2 = 
  List.map2 (fun exp1 exp2 -> expr_eq exp1 exp2) my_exp_list_1 my_exp_list_2;;

  let exp_list_equal_second my_exp_list_1 my_exp_list_2 = 
    List.map2 (fun exp1 exp2 -> expr_eq (List.hd (Tag_Parser.tag_parse_expressions exp1)) exp2) my_exp_list_1 my_exp_list_2;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
  [Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1,1)), Nil)), Pair (Pair (Symbol "y", Pair (Number (Fraction (2, 1)), Nil)), Nil)), Pair (Symbol "y", Nil)))]
)
[Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil)))]
)[Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1))))), [])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction (1, 1)), Nil)), Nil), Pair (Symbol "x", Nil)))]
)
[Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Fraction (1, 1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3,1)), Nil)))), Nil)))]
)
[Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction (1, 1)))); Const (Sexpr (Number (Fraction (2, 1))));Const (Sexpr (Number (Fraction (3, 1))))]),[])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "b", Pair (Number (Fraction (10, 1)), Nil)), Nil)), Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))]
)
[Applic (LambdaSimple (["a"; "b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction (3, 1)))); Const (Sexpr (Number (Fraction (10, 1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil), Pair (Symbol "x", Nil)))]
)
[Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "t", Pair (Bool true, Nil)), Pair (Pair (Symbol "th", Pair (Number (Fraction (3, 1)), Nil)), Pair (Pair (Symbol "el", Pair (Number (Fraction (4, 1)), Nil)), Nil))), Pair (Pair (Symbol "if", Pair (Symbol "t", Pair (Symbol "th", Pair (Symbol "el", Nil)))), Nil)))]
)
[Applic (LambdaSimple (["t"; "th"; "el"], If (Var "t", Var "th", Var "el")),[Const (Sexpr (Bool true)); Const (Sexpr (Number (Fraction (3, 1))));Const (Sexpr (Number (Fraction (4 , 1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Nil)))]
)[Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 1.23))))),[Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Pair (Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction (-1, 1)), Nil))), Nil))), Nil)))]
)
[Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction (-1, 1))))])]),[Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))]
)
[Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil))  ]
)
[Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil))  ]
)
[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))]
)
[Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))]
)
[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))]
)
[Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))]
)
[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))]
)
[Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))]
)
[Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))]
)
[Applic (Var "append", [Var "a"; Var "b"])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),Nil))]
)
[Applic (Var "cons", [Var "a"; Var "b"])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quasiquote",Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))]
)
[Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda", Pair (Nil, Pair (Number (Fraction (10, 1)), Nil))) ]
)
[LambdaSimple ([], Const (Sexpr (Number (Fraction (10, 1)))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Symbol "a", Nil)))  ]
)[LambdaSimple (["a"; "b"], Var "a")]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Nil)))  ]
)
[LambdaSimple (["a"], Var "a")]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))]
)
[LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"]))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "if",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))]
)
[LambdaSimple (["x"; "y"; "z"], If (Var "x", Var "y", Var "z"))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))]
)
[LambdaSimple (["x"; "y"; "z"], Seq [Var "x"; Var "y"; Var "z"])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "set", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))]
)
[LambdaSimple (["x"; "y"], Applic (Var "set", [Var "x"; Var "y"]))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair(Pair (Symbol "x",Pair (Symbol "y", Pair (Symbol "z", Pair (Symbol "w", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "x",Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "z", Pair (Symbol "w", Nil))), Nil)))),Nil)))]
)
[LambdaSimple (["x"; "y"; "z"; "w"],If (Var "x", Applic (Var "+", [Var "y"; Var "z"]),Applic (Var "+", [Var "z"; Var "w"])))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",                                                           Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))]
)
[LambdaOpt (["x"; "y"], "vs", Seq [Var "x"; Var "y"; Var "vs"])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",                                                           Pair (Pair (Symbol "x", Symbol "vs"),Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "vs", Nil))), Nil)))]
)
[LambdaOpt (["x"], "vs", If (Var "x", Var "vs", Const Void))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)), Nil)),Pair (Symbol "y", Nil)))]
)
[Applic(LambdaSimple (["x"],Applic (LambdaSimple (["y"], Var "y"), [Const (Sexpr (Number (Fraction(2,1))))])),[Const (Sexpr (Number (Fraction(1,1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)),Pair (Pair (Symbol "z", Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Symbol "a", Pair (Number (Fraction(4,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(5,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(6,1)), Nil)), Nil)))))),Pair(Pair (Symbol "begin",Pair (Symbol "x",Pair (Symbol "y",Pair (Symbol "z",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))))),Nil)))]
)
[Applic(LambdaSimple (["x"],Applic(LambdaSimple (["y"],Applic(LambdaSimple (["z"],Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Seq [Var "x"; Var "y"; Var "z"; Var "a"; Var "b"; Var "c"]),[Const (Sexpr (Number (Fraction(6,1))))])),[Const (Sexpr (Number (Fraction(5,1))))])),[Const (Sexpr (Number (Fraction(4,1))))])),[Const (Sexpr (Number (Fraction(3,1))))])),[Const (Sexpr (Number (Fraction(2,1))))])),[Const (Sexpr (Number (Fraction(1,1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(2,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Symbol "d", Pair (Number (Fraction(4,1)), Nil)),Pair (Pair (Symbol "e", Pair (Number (Fraction(5,1)), Nil)),Pair (Pair (Symbol "f", Pair (Number (Fraction(5,1)), Nil)),Pair (Pair (Symbol "g", Pair (Number (Fraction(6,1)), Nil)), Nil))))))),Pair(Pair (Symbol "and",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Pair (Symbol "d",Pair (Symbol "e", Pair (Symbol "f", Pair (Symbol "g", Nil)))))))),Nil)))]
)
[Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Applic(LambdaSimple (["d"],Applic(LambdaSimple (["e"],Applic(LambdaSimple (["f"],Applic(LambdaSimple (["g"],If (Var "a",If (Var "b",If (Var "c",If (Var "d",If (Var "e",If (Var "f", Var "g", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))),[Const (Sexpr (Number (Fraction(6,1))))])),[Const (Sexpr (Number (Fraction(5,1))))])),[Const (Sexpr (Number (Fraction(5,1))))])),[Const (Sexpr (Number (Fraction(4,1))))])),[Const (Sexpr (Number (Fraction(3,1))))])),[Const (Sexpr (Number (Fraction(2,1))))])),[Const (Sexpr (Number (Fraction(1,1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair (Nil,Pair(Pair (Symbol "begin",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))]
)
[Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))]),[])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)), Nil)),Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))]
)
[Applic(LambdaSimple (["a"],Applic (LambdaSimple (["b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(3,1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil),Pair (Symbol "x", Nil)))]
)
[Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair(Pair (Pair (Symbol "t", Pair (Bool true, Nil)),Pair (Pair (Symbol "th", Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Symbol "el", Pair (Number (Fraction(4,1)), Nil)), Nil))),Pair(Pair (Symbol "if",Pair (Bool true, Pair (Number (Fraction(3,1)), Pair (Number (Fraction(4,1)), Nil)))),Nil)))]
)
[Applic(LambdaSimple (["t"],Applic(LambdaSimple (["th"],Applic(LambdaSimple (["el"],If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Fraction(3,1)))),Const (Sexpr (Number (Fraction(4,1)))))),[Const (Sexpr (Number (Fraction(4,1))))])),[Const (Sexpr (Number (Fraction(3,1))))])),[Const (Sexpr (Bool true))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 12.3), Nil))),Nil)))]
)
[Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 12.3))))),[Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "begin",Pair(Pair (Symbol "define",Pair (Symbol "y", Pair (Number (Float 1.23), Nil))),Pair(Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction(-1,1)), Nil))),Nil))),Nil)))]
)
[Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction(-1,1))))])]),[Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "let*",                                                             Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))]
)
[Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "cond",                                                             Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair(Pair (Number (Fraction(4,1)), Pair (Number (Fraction(5,1)), Pair (Number (Fraction(6,1)), Nil))),Nil)))]
)
[If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(2,1)))); Const (Sexpr (Number (Fraction(3,1))))],If (Const (Sexpr (Number (Fraction(4,1)))),Seq [Const (Sexpr (Number (Fraction(5,1)))); Const (Sexpr (Number (Fraction(6,1))))],Const Void))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "cond",                                                             Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair(Pair (Number (Fraction(4,1)), Pair (Number (Fraction(5,1)), Pair (Number (Fraction(6,1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(7,1)), Pair (Number (Fraction(8,1)), Pair (Number (Fraction(9,1)), Nil)))),Nil))))]
)
[If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(2,1)))); Const (Sexpr (Number (Fraction(3,1))))],If (Const (Sexpr (Number (Fraction(4,1)))),Seq [Const (Sexpr (Number (Fraction(5,1)))); Const (Sexpr (Number (Fraction(6,1))))],Seq[Const (Sexpr (Number (Fraction(7,1)))); Const (Sexpr (Number (Fraction(8,1))));Const (Sexpr (Number (Fraction(9,1))))]))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "cond",                                                             Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(4,1)), Pair (Number (Fraction(5,1)), Pair (Number (Fraction(6,1)), Nil)))),Pair(Pair (Number (Fraction(7,1)), Pair (Number (Fraction(8,1)), Pair (Number (Fraction(9,1)), Nil))),Nil))))]
)
[If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(2,1)))); Const (Sexpr (Number (Fraction(3,1))))],Seq[Const (Sexpr (Number (Fraction(4,1)))); Const (Sexpr (Number (Fraction(5,1))));Const (Sexpr (Number (Fraction(6,1))))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "cond",                                                             Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil)) ]
)
[Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "cond",                                                             Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))]
)
[Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))])])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "and",                                                              Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)),Pair (Number (Fraction(3,1)),Pair (Number (Fraction(4,1)),Pair (Number (Fraction(5,1)),Pair (Number (Fraction(6,1)),Pair (Number (Fraction(7,1)),Pair (Number (Fraction(8,1)),Pair (Number (Fraction(9,1)), Pair (Number (Fraction(10,1)), Nil)))))))))))]
)
[If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(2,1)))),If (Const (Sexpr (Number (Fraction(3,1)))),If (Const (Sexpr (Number (Fraction(4,1)))),If (Const (Sexpr (Number (Fraction(5,1)))),If (Const (Sexpr (Number (Fraction(6,1)))),If (Const (Sexpr (Number (Fraction(7,1)))),If (Const (Sexpr (Number (Fraction(8,1)))),If (Const (Sexpr (Number (Fraction(9,1)))), Const (Sexpr (Number (Fraction(10,1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "*", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))]
)
[Def (Var "square",LambdaSimple (["x"], Applic (Var "*", [Var "x"; Var "x"])))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Symbol "d"))),Pair(Pair (Symbol "quasiquote",Pair(Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),Pair(Pair (Symbol "b",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Pair(Pair (Symbol "c",Pair (Pair (Symbol "unquote", Pair (Symbol "c", Nil)), Nil)),Pair(Pair (Symbol "d",Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)), Nil)),Nil)))),Nil)),Nil)))]
)
[LambdaOpt (["a"; "b"; "c"], "d",Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "a"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "b"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "c"));Applic (Var "cons", [Var "c"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "d"));Applic (Var "cons", [Var "d"; Const (Sexpr Nil)])]);Const (Sexpr Nil)])])])]))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction(1,1)), Pair (Number (Fraction((-1),1)), Nil)))),Nil)))]
)
[Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction(1,1)))),Const (Sexpr (Number (Fraction((-1),1)))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "*",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))]
)
[Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "*", [Var "a"; Var "b"; Var "c"])))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Nil))]
)
[Const(Sexpr(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Number (Float 3.4)), Nil))]
)
[Const (Sexpr (Pair (Number (Float 1.2), Number (Float 3.4))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.)))),Nil))]
)
[Const(Sexpr(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555))))),Nil))]
)
[Const(Sexpr(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555)))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535)))),Nil))]
)
[Const(Sexpr(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote", Pair (Pair (Number (Float 0.123), Nil), Nil))]
)
[Const (Sexpr (Pair (Number (Float 0.123), Nil)))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote", Pair (Pair (Char '\000', Nil), Nil))]
)
[Const (Sexpr (Pair (Char '\000', Nil)))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Char '\n']
)
[Const (Sexpr (Char '\n'))]
;;

exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil)))),Nil))]
)
[Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil))),Nil))]
)
[Const(Sexpr(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil)))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e"))),Nil))]
)
[Const(Sexpr(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e")))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000'))),Nil))]
)
[Const(Sexpr(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000')))))]

;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (String "should not", Pair (String "be", Pair (String "list", Nil)))]
)
[Applic (Const (Sexpr (String "should not")),[Const (Sexpr (String "be")); Const (Sexpr (String "list"))])]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair (Pair (String "should", Pair (String "be", String "list")), Nil))]
)
[Const (Sexpr (Pair (String "should", Pair (String "be", String "list"))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex", Pair (Char '\r', Pair (String "list", Nil)))))))),Nil))]

)
[Const(Sexpr(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex",Pair (Char '\r', Pair (String "list", Nil))))))))))]
;;


exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g"))))),Nil))]
)
[Const(Sexpr(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g")))))))]
;;

exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' ')))),Nil))]

)
[Const(Sexpr(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' '))))))]

;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(5,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f")))))),Nil))]
)
[Const(Sexpr(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(5,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f"))))))))]
;;


exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1)))))))),Nil))]
)
[Const(Sexpr(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1))))))))))]
;;


exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z")))),Nil))]
)
[Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z"))))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))]
)
[Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))]
;;


exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))]
)
[Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))]
;;

exp_list_equal
(Tag_Parser.tag_parse_expressions
  [Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))]
)
[Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))]
;;

exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))]
)
[Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))]
;;

exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil)), Nil))]
)
[Const (Sexpr (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil))))]
;;
exp_list_equal
(Tag_Parser.tag_parse_expressions
[Pair (Symbol "quote", Pair(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000'))),Nil))]
)
[Const(Sexpr(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000')))))]
;;

