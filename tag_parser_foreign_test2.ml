#use "tag-parser.ml";;
(*open Tag_Parser;;*)


expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "123"))) (Const(Sexpr (Number (Fraction (123, 1)))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "\"check the string\""))) (Const (Sexpr (String "check the string")));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "#\\c"))) (Const(Sexpr (Char 'c')));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "#f"))) (Const(Sexpr (Bool false)));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "#T"))) (Const(Sexpr (Bool true)));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "'#t'"))) (Const(Sexpr (Bool true)));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "'123"))) (Const(Sexpr (Number (Fraction (123, 1)))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "''123"))) (Const(Sexpr (Pair (Symbol "quote", Pair (Number (Fraction (123, 1)), Nil)))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "x"))) (Var("x"));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(if a b c)"))) (If (Var "a", Var "b", Var "c"));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(if a b)"))) (If (Var "a", Var "b", Const Void));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin 1 2 3)"))) (Seq
  [Const (Sexpr (Number (Fraction (1, 1))));
   Const (Sexpr (Number (Fraction (2, 1))));
   Const (Sexpr (Number (Fraction (3, 1))))]);;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(set! x 3)"))) (Set (Var "x", Const (Sexpr (Number (Fraction (3, 1))))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(define y 4)"))) (Def (Var "y", Const (Sexpr (Number (Fraction (4, 1))))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(or)"))) (Const(Sexpr(Bool(false))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(or 3)"))) (Const(Sexpr(Number(Fraction(3,1)))));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(or #f #f #t)"))) (Or
  [Const (Sexpr (Bool false)); Const (Sexpr (Bool false));
   Const (Sexpr (Bool true))]);;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(foo 1 a b)"))) (Applic (Var "foo",
  [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"; Var "b"]));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(lambda c c)"))) (LambdaOpt ([], "c", Var "c"));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(lambda (a . b) b)"))) (LambdaOpt (["a"], "b", Var "b"));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(lambda (a b . c) c)"))) (LambdaOpt (["a";"b"], "c", Var "c"));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(lambda (a b c) (+(+ a b)c)))"))) (LambdaSimple (["a"; "b"; "c"],
  Applic (Var "+", [Applic (Var "+", [Var "a"; Var "b"]); Var "c"])));;
expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(lambda () 1)"))) (LambdaSimple ([],
 Const(Sexpr(Number(Fraction(1,1))))));;
 expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin a (begin b c))"))) (Seq [Var "a"; Var "b"; Var "c"]);;
 expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin a b (begin c))"))) (Seq [Var "a"; Var "b"; Var "c"]);;
 expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin a b (begin c d (begin e f)))"))) (Seq [Var "a"; Var "b"; Var "c";Var "d"; Var "e"; Var "f"]);;
 expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin a (lambda () b c))"))) ( Seq [ Var "a"; LambdaSimple
([], Seq[Var "b"; Var "c"])]);;
 expr_eq (List.hd (Tag_Parser.tag_parse_expressions (read_sexprs "(begin a (begin b (begin c (lambda () d (begin e f) g) h)))"))) (Seq[Var "a"; Var "b"; Var "c"; LambdaSimple ([], Seq[Var "d"; Var "e";
Var "f"; Var "g"]); Var "h"]);;