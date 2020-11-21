#use "reader.ml";;
open Reader;;

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

let test_exp res expected =
  if sexpr_eq res expected
  then true
  else false;;

exception TestFail_Result_Ended_Before_Expected;;  
exception Test_Fail_No_Match;;

let test_sexps_lists name lst1 lst2 = 
  let func = 
    (fun acc b -> 
       match acc with
       | [] -> Printf.printf "Test: %s -> Fail\n\tResult Ended, But Expected: %s\n" name (unread b);
              raise TestFail_Result_Ended_Before_Expected
       | a::res1 -> if (test_exp a b)
         then (res1)
         else ([];
               Printf.printf "Test: %s -> Fail:\n\tGot: %s\n\tExpected: %s\n\t" name (unread a) (unread b);
               raise Test_Fail_No_Match)
    ) in
  List.fold_left func lst1 lst2;
  Printf.printf "Test: %s -> Success" name;;




test_sexps_lists "Example#1" ([Bool(true)]) ([Bool(true)]);;

test_sexps_lists "Boolean#1" (read_sexprs(" #t  ")) ([Bool(true)]);;
test_sexps_lists "Boolean#2" (read_sexprs(" \012 #T  ")) ([Bool(true)]);;
test_sexps_lists "Boolean#3" (read_sexprs("#f  ")) ([Bool(false)]);;
test_sexps_lists "Boolean#4" (read_sexprs("\n#F  ")) ([Bool(false)]);;
test_sexps_lists "Boolean#5" (read_sexprs("\n#t  #t  ")) ([Bool(true); Bool(true)]);;

test_sexps_lists "Number#1" (read_sexprs("1")) ([Number(Fraction(1,1))]);;
test_sexps_lists "Number#2" (read_sexprs("  1")) ([Number(Fraction(1,1))]);;
test_sexps_lists "Number#3" (read_sexprs("  1 \t ")) ([Number(Fraction(1,1))]);;
test_sexps_lists "Number#4" (read_sexprs("01234")) ([Number(Fraction(1234,1))]);;
test_sexps_lists "Number#5" (read_sexprs("001234")) ([Number(Fraction(1234,1))]);;
test_sexps_lists "Number#6" (read_sexprs("-01234")) ([Number(Fraction(-1234,1))]);;
test_sexps_lists "Number#7" (read_sexprs("+01234")) ([Number(Fraction(1234,1))]);;
test_sexps_lists "Number#8" (read_sexprs("+00940")) ([Number(Fraction(940,1))]);;
test_sexps_lists "Number#9" (read_sexprs("+00940000")) ([Number(Fraction(940000,1))]);;

test_sexps_lists "Fraction#1" (read_sexprs("1/1")) ([Number(Fraction(1,1))]);;
test_sexps_lists "Fraction#2" (read_sexprs("2/4")) ([Number(Fraction(1,2))]);;
test_sexps_lists "Fraction#3" (read_sexprs("-17/6")) ([Number(Fraction(-17,6))]);;
test_sexps_lists "Fraction#4" (read_sexprs("+006/012")) ([Number(Fraction(1,2))]);;
test_sexps_lists "Fraction#5" (read_sexprs("-003/0150")) ([Number(Fraction(-1,50))]);;
test_sexps_lists "Fraction#6" (read_sexprs("    -003/0150     ")) ([Number(Fraction(-1,50))]);;
test_sexps_lists "Fraction#7" (read_sexprs("0/7")) ([Number(Fraction(0,1))]);;

test_sexps_lists "Float#1" (read_sexprs("111.1")) ([Number(Float(111.1))]);;
test_sexps_lists "Float#2" (read_sexprs("111.100")) ([Number(Float(111.1))]);;
test_sexps_lists "Float#3" (read_sexprs("-111.001")) ([Number(Float(-111.001))]);;
test_sexps_lists "Float#4" (read_sexprs("+006.012")) ([Number(Float(6.012))]);;
test_sexps_lists "Float#5" (read_sexprs("-3.0150")) ([Number(Float(-3.015))]);;
test_sexps_lists "Float#6" (read_sexprs("       -3.0150      ")) ([Number(Float(-3.015))]);;


test_sexps_lists "Scientific#1" (read_sexprs("1e1")) ([Number(Float(10.))]);;
test_sexps_lists "Scientific#2" (read_sexprs("1E+1")) ([Number(Float(10.))]);;
test_sexps_lists "Scientific#3" (read_sexprs("10e-1")) ([Number(Float(1.))]);;
test_sexps_lists "Scientific#4" (read_sexprs("-10e-1")) ([Number(Float(-1.))]);;
test_sexps_lists "Scientific#5" (read_sexprs("3.14e+9")) ([Number(Float(3140000000.))]);;
test_sexps_lists "Scientific#6" (read_sexprs("+000000012.3E00000002")) ([Number(Float(1230.))]);;
test_sexps_lists "Scientific#7" (read_sexprs("3.14E-512")) ([Number(Float(0.))]);;
test_sexps_lists "Scientific#8" (read_sexprs("5e-2")) ([Number(Float(0.05))]);;
test_sexps_lists "Scientific#9" (read_sexprs("      5e-2      ")) ([Number(Float(0.05))]);;

test_sexps_lists "Number_Before_Symbol#1" (read_sexprs("      5e -2      ")) ([Symbol("5e"); Number(Fraction(-2,1))]);;
test_sexps_lists "Number_Before_Symbol#2" (read_sexprs(" 5 e -2 ")) ([Number(Fraction(5,1)); Symbol("e"); Number(Fraction(-2,1))]);;
test_sexps_lists "Number_Before_Symbol#3" (read_sexprs("+3.+2")) ([Symbol("+3.+2")]);;

test_sexps_lists "Symbol#1" (read_sexprs(" 1a^  ")) ([Symbol("1a^")]);;
test_sexps_lists "Symbol#2" (read_sexprs(" 1a^<:  ")) ([Symbol("1a^<:")]);;
test_sexps_lists "Symbol#3" (read_sexprs("AbC")) ([Symbol("abc")]);;
test_sexps_lists "Symbol#4" (read_sexprs("a1+3====1.1")) ([Symbol("a1+3====1.1")]);;
test_sexps_lists "Symbol#5" (read_sexprs("..")) ([Symbol("..")]);;
test_sexps_lists "Symbol#6" (read_sexprs("..123Ac^;comment")) ([Symbol("..123ac^")]);;
test_sexps_lists "Symbol#7" (read_sexprs("123a")) ([Symbol("123a")]);;
test_sexps_lists "Symbol#8" (read_sexprs("!$^*-_=+<>/? ")) ([Symbol("!$^*-_=+<>/?")]);;
test_sexps_lists "Symbol#9" (read_sexprs(".!$^*-_=+<>/? w1")) ([Symbol(".!$^*-_=+<>/?"); Symbol("w1")]);;
test_sexps_lists "Symbol#10" (read_sexprs("0.!$^*-_=+<>/? .w")) ([Symbol("0.!$^*-_=+<>/?"); Symbol(".w")]);;
test_sexps_lists "Symbol#11" (read_sexprs("a0.!$^*-_=+<>/?b")) ([Symbol("a0.!$^*-_=+<>/?b")]);;
test_sexps_lists "Symbol#12" (read_sexprs("(..... . ....)")) ([Pair(Symbol("....."), Symbol("...."))]);;
test_sexps_lists "Symbol#13" (read_sexprs("1.2a")) ([Symbol("1.2a")]);;
test_sexps_lists "Symbol#13" (read_sexprs("1.2 a")) ([Number(Float(1.2)); Symbol("a")]);;
test_sexps_lists "Symbol#14" (read_sexprs("10.")) ([Symbol("10.")]);;
test_sexps_lists "Symbol#15" (read_sexprs(" '(a 1 . a) ")) 
([Pair(Symbol("quote"),Pair(Pair(Symbol("a"),Pair(Number(Fraction(1,1)),Symbol("a"))),Nil))]);;

test_sexps_lists "String#1" (read_sexprs("\"123a\"")) ([(String("123a"))]);;
test_sexps_lists "String#2" (read_sexprs("\"a\\fb\\nc\\nd\"")) ([(String("a\012b\nc\nd"))]);;
test_sexps_lists "String#3" (read_sexprs("\"\na\fb\\nc\\nd\t\"")) ([(String("\na\012b\nc\nd\t"))]);;
test_sexps_lists "String#4" (read_sexprs("\"\r\"")) ([(String("\r"))]);;
test_sexps_lists "String#5" (read_sexprs("\"f\"")) ([(String("f"))]);;

test_sexps_lists "String#6" (read_sexprs("\"A\\FB\\NC\\ND\"")) ([(String("A\012B\nC\nD"))]);;
test_sexps_lists "String#7" (read_sexprs("\"\NA\FB\\NC\\ND\T\"")) ([(String("\nA\012B\nC\nD\t"))]);;
test_sexps_lists "String#8" (read_sexprs("\"\\\\ \\\" \\R\\N\\T\\F\"")) ([(String("\\ \" \r\n\t\012"))]);;
test_sexps_lists "String#9" (read_sexprs("\"\R\"")) ([(String("\r"))]);;
test_sexps_lists "String#10" (read_sexprs("\"F\"")) ([(String("F"))]);;
test_sexps_lists "String#11" (read_sexprs("\"\\\\F\"")) ([(String("\\F"))]);;

test_sexps_lists "String#12" (read_sexprs("\"\\fAB\"")) ([String("\012AB")]);;
test_sexps_lists "String#13" (read_sexprs("\"afB\"")) ([(String("afB"))]);;
test_sexps_lists "String#14" (read_sexprs("\"(afB)\"")) ([(String("(afB)"))]);;
test_sexps_lists "String#15" (read_sexprs("\"(afB)\"")) ([(String("(afB)"))]);;
test_sexps_lists "String#16" (read_sexprs("\"\\\\\"")) ([String("\\")]);;

test_sexps_lists "Char#1" (read_sexprs(" #\\newline\n")) ([Char(char_of_int 10)]);;
test_sexps_lists "Char#2" (read_sexprs("   #\\nul ")) ([Char(char_of_int 0)]);;
test_sexps_lists "Char#3" (read_sexprs("   #\\return\r")) ([Char(char_of_int 13)]);;
test_sexps_lists "Char#4" (read_sexprs("   #\\tab\t")) ([Char(char_of_int 9)]);;
test_sexps_lists "Char#5" (read_sexprs("   #\\page  ")) ([Char(char_of_int 12)]);;
test_sexps_lists "Char#6" (read_sexprs("   #\\space   ")) ([Char(' ')]);;
test_sexps_lists "Char#7" (read_sexprs(" #\\a ")) ([Char('a')]);;
test_sexps_lists "Char#8" (read_sexprs(" #\\A ")) ([Char('A')]);;
test_sexps_lists "Char#9" (read_sexprs(" #\\r ")) ([Char('r')]);;
test_sexps_lists "Char#10" (read_sexprs(" #\\t ")) ([Char('t')]);;
test_sexps_lists "Char#11" (read_sexprs(" #\\? ")) ([Char('?')]);;

test_sexps_lists "List#1" (read_sexprs("(1 1)")) ([Pair(Number(Fraction(1,1)),
                                                        Pair(Number(Fraction(1,1)), Nil))]);;
test_sexps_lists "List#2" (read_sexprs("(1 . 1)")) ([Pair(Number(Fraction(1,1)),
                                                          Number(Fraction(1,1)))]);;
test_sexps_lists "List#3" (read_sexprs("( 1.23e-2 a!^< (sym1 2sym))")) 
  ([Pair(Number(Float(0.0123)),
         Pair(Symbol("a!^<"), 
              Pair(
                Pair(Symbol("sym1"),
                     Pair(Symbol("2sym"), Nil)), 
                Nil)))]);;

test_sexps_lists "List#4" (read_sexprs("\t( \"str1\" . \"str2\\\"\" )")) ([Pair(String("str1"),String("str2\""))]);;
test_sexps_lists "List#5" (read_sexprs("(() ())")) ([Pair(Nil,Pair(Nil,Nil))]);;
test_sexps_lists "List#6" (read_sexprs("(1.1 . (1.2 . (1.3 . ())))")) ([Pair(Number(Float (1.1)), Pair(Number(Float(1.2)),Pair(Number(Float(1.3)),Nil)))]);;
test_sexps_lists "List#7" (read_sexprs(" ( 1.1 1.2 1.3 \t)  ")) ([Pair(Number(Float (1.1)), Pair(Number(Float(1.2)),Pair(Number(Float(1.3)),Nil)))]);;
test_sexps_lists "List#8" (read_sexprs("()")) ([Nil]);;


test_sexps_lists "QuoteBasic#1" (read_sexprs("'1")) ([Pair(Symbol("quote"), Pair(Number(Fraction(1,1)), Nil))]);;
test_sexps_lists "QuoteBasic#2" (read_sexprs("`1")) ([Pair(Symbol("quasiquote"), Pair(Number(Fraction(1,1)), Nil))]);;
test_sexps_lists "QuoteBasic#3" (read_sexprs(",1")) ([Pair(Symbol("unquote"), Pair(Number(Fraction(1,1)), Nil))]);;
test_sexps_lists "QuoteBasic#4" (read_sexprs(",@1")) ([Pair(Symbol("unquote-splicing"), Pair(Number(Fraction(1,1)), Nil))]);;
test_sexps_lists "QuoteBasic#5" (read_sexprs("'3+2")) ([Pair(Symbol("quote"), Pair(Symbol("3+2"), Nil))]);;

test_sexps_lists "Quote#1" (read_sexprs(",@e")) ([Pair(Symbol("unquote-splicing"), Pair(Symbol("e"), Nil))]);;
test_sexps_lists "Quote#3" (read_sexprs(",@,@5")) ([Pair(Symbol("unquote-splicing"), Pair(    Pair(Symbol("unquote-splicing"), Pair(    Number(Fraction(5,1))      , Nil))      , Nil))]);;
test_sexps_lists "Quote#4" (read_sexprs("'(a 1 . a)")) ([Pair(Symbol("quote"), Pair( Pair(Symbol("a"), Pair(Number(Fraction(1,1)), Symbol("a"))) , Nil))]);;
test_sexps_lists "Quote#4" (read_sexprs("`(1+2)")) ([Pair(Symbol "quasiquote", Pair(Pair(Symbol "1+2", Nil), Nil))]);;
test_sexps_lists "Quote#4" (read_sexprs("`(1 + 2)")) ([Pair(Symbol "quasiquote", Pair(Pair(Number (Fraction (1, 1)), Pair(Symbol "+", Pair(Number (Fraction (2, 1)), Nil))), Nil))]);;


test_sexps_lists "Comment#1" (read_sexprs(";testing a <>?<>?: comment\n+5.000000e1;comment!!")) ([Number(Float(50.0))]);;
test_sexps_lists "Comment#2" (read_sexprs("(1.1 #;(1.2 1.3) )")) ([Pair(Number(Float (1.1)),Nil)]);;
test_sexps_lists "Comment#3" (read_sexprs("'(a #;1 . a)")) ([Pair(Symbol("quote"), Pair(Pair(Symbol("a"),Symbol("a")) , Nil))]);;
test_sexps_lists "Comment#4" (read_sexprs("'(a a #;r)")) ([Pair(Symbol("quote"),  Pair(Pair(Symbol("a"),Pair(Symbol"a",Nil)), Nil))]);;
test_sexps_lists "Comment#5" (read_sexprs("'(a a #;r)")) ([Pair(Symbol("quote"),  Pair(Pair(Symbol("a"),Pair(Symbol"a",Nil)), Nil))]);;
test_sexps_lists "Comment#6" (read_sexprs("#;#;(+ 1 2) 1 \"Only one left\"")) ([String("Only one left")]);;
test_sexps_lists "Comment#7" (read_sexprs("#;  (1 2 3) 1 #; #; 2 2 2")) ([Number(Fraction(1,1)); Number(Fraction(2,1))]);;
test_sexps_lists "Comment#8" (read_sexprs("a a; comment comment\n1 2")) ([Symbol("a"); Symbol("a"); Number(Fraction(1,1)); Number(Fraction(2,1))]);;
test_sexps_lists "Comment#9" (read_sexprs("( 1;comment\n . 2;comment\n)")) ([Pair(Number(Fraction(1,1)), Number(Fraction(2,1)))]);;
test_sexps_lists "Comment#10" (read_sexprs("(#; ;line comment\n1\n2)")) ([Pair(Number(Fraction(2,1)),Nil)]);;
test_sexps_lists "Comment#11" (read_sexprs("#; ;comment \n #; \"Hello\" 123 #t")) ([Bool(true)]);;
test_sexps_lists "Comment#12" (read_sexprs("(#;1\n)")) ([Nil]);;
test_sexps_lists "Comment#13" (read_sexprs("(;bla\n)")) ([Nil]);;
test_sexps_lists "Comment#14" (read_sexprs("(#;1#;2 sym #;2\n)")) ([Pair(Symbol("sym"), Nil)]);;
test_sexps_lists "Comment#15" (read_sexprs("(#;1 sym #;2 #;2\n)")) ([Pair(Symbol("sym"), Nil)]);;

(* These won't be tested *)
test_sexps_lists "Empty#1" (read_sexprs("")) ([]);;
test_sexps_lists "Empty#2" (read_sexprs(";comment")) ([]);;
test_sexps_lists "Empty#3" (read_sexprs("#;2")) ([]);;

(* Several SEXPRs *)
test_sexps_lists "Sequence#1" (read_sexprs("1 2 3")) ([Number(Fraction(1,1)); Number(Fraction(2,1)); Number(Fraction(3,1))]);;
test_sexps_lists "Sequence#2" (read_sexprs("a 2 'b")) ([Symbol("a"); Number(Fraction(2,1)); Pair(Symbol("quote"), Pair(Symbol("b"), Nil))]);;
test_sexps_lists "Sequence#3" (read_sexprs("(1 . 3) (a b)")) 
  ([Pair(Number(Fraction(1,1)), Number(Fraction(3,1)));
    Pair(Symbol("a"), Pair(Symbol("b"), Nil))]);;
test_sexps_lists "Sequence#11" (read_sexprs("\012#t3\t()")) ([Bool(true); Number(Fraction(3, 1)); Nil]);;
test_sexps_lists "Sequence#12" (read_sexprs("a. .b")) ([Symbol("a."); Symbol(".b")]);;
test_sexps_lists "Sequence#13" (read_sexprs("1 / 3")) ([Number(Fraction(1,1)); Symbol("/"); Number(Fraction(3,1))]);;
test_sexps_lists "Sequence#14" (read_sexprs("(1 . 2 )(3 . 4)")) ([Pair(Number(Fraction(1,1)), Number(Fraction(2,1))); Pair(Number(Fraction(3,1)), Number(Fraction(4,1)))]);;
test_sexps_lists "Sequence#15" (read_sexprs("#\\; f\ne")) ([Char(';'); Symbol("f"); Symbol("e")]);;

(* Official tests from the assignment of 2020 *)
test_sexps_lists "1" (read_sexprs("#t")) [Bool true];;
test_sexps_lists "2" (read_sexprs("#F")) [Bool false];;
test_sexps_lists "3" (read_sexprs("#\\newline")) [Char '\n'];;
test_sexps_lists "4" (read_sexprs("#\\RETURN")) [Char '\r'];;
test_sexps_lists "5" (read_sexprs("#\\Nul")) [Char '\000'];;
test_sexps_lists "6" (read_sexprs("#\\a")) [Char 'a'];;
test_sexps_lists "7" (read_sexprs("#\\?")) [Char '?'];;
test_sexps_lists "8" (read_sexprs("#\\Ab")) [Char 'A'; Symbol "b"];;
test_sexps_lists "9" (read_sexprs("1")) [Number (Fraction (1, 1))];;
test_sexps_lists "10" (read_sexprs("  -123")) [Number (Fraction (-123, 1))];;
test_sexps_lists "11" (read_sexprs("0001")) [Number (Fraction (1,1))];;
test_sexps_lists "12" (read_sexprs("1.2")) [Number (Float 1.2)];;
test_sexps_lists "13" (read_sexprs("-0.9")) [Number (Float (-0.9))];;
test_sexps_lists "14" (read_sexprs("+1.23456789")) [Number (Float 1.23456789)];;
test_sexps_lists "15" (read_sexprs("1   2")) [Number (Fraction(1, 1)); Number (Fraction(2, 1))];;
test_sexps_lists "16" (read_sexprs("0.1    0.2")) [Number (Float 0.1); Number (Float 0.2)];;
test_sexps_lists "17" (read_sexprs("9.9 ;comment\n")) [Number (Float 9.9)];;

(* test_sexps_lists "18" (read_sexprs("#16R11.8a")) [Number (Float 17.5390625)];;
test_sexps_lists "19" (read_sexprs("#2r-1101")) [Number (Fraction (-13, 1))];;
test_sexps_lists "20" (read_sexprs("#2r+1101")) [Number (Fraction(13, 1))];; *)

test_sexps_lists "21" (read_sexprs("10e-1")) [Number (Float 1.)];;
test_sexps_lists "22" (read_sexprs("1e1")) [Number (Float 10.)];;
test_sexps_lists "23" (read_sexprs("3.14e+9")) [Number (Float 3140000000.)];;
test_sexps_lists "24" (read_sexprs("1 ;HelloWorld\n")) [Number (Fraction(1, 1))];;
test_sexps_lists "25" (read_sexprs("(;Is it List?\n)")) [Nil];;
test_sexps_lists "26" (read_sexprs("(    #;#t    )")) [Nil];;
test_sexps_lists "27" (read_sexprs(";comment")) [];;
test_sexps_lists "28" (read_sexprs("(1 2)")) [Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Nil))];;
test_sexps_lists "29" (read_sexprs("(1.2 . 3)")) [Pair (Number (Float 1.2), Number (Fraction(3,1)))];;
test_sexps_lists "30" (read_sexprs("((1 2) . 3)")) [Pair (Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Nil)), Number (Fraction(3,1)))];;
test_sexps_lists "31" (read_sexprs("(a . b)")) [Pair (Symbol "a", Symbol "b")];;
test_sexps_lists "32" (read_sexprs("(a b c)")) [Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))];;
test_sexps_lists "33" (read_sexprs("(a b . c)")) [Pair (Symbol "a", Pair (Symbol "b", Symbol "c"))];;
test_sexps_lists "34" (read_sexprs("(a . (b . (c . (d . e))))")) [Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "e"))))];;
test_sexps_lists "35" (read_sexprs("((1 (a)) b)")) [Pair (Pair (Number (Fraction(1,1)), Pair (Pair (Symbol "a", Nil), Nil)),Pair (Symbol "b", Nil))];;
test_sexps_lists "36" (read_sexprs("(\"str\" ; comment \n)")) [Pair (String "str", Nil)];;
test_sexps_lists "37" (read_sexprs("\"Hello World\"")) [String "Hello World"];;
test_sexps_lists "38" (read_sexprs("\"Hello \\t \\r \\n world!\"")) [String "Hello \t \r \n world!"];;
test_sexps_lists "39" (read_sexprs("\"Hello \\\\t \\\\r \\n world!\"")) [String "Hello \\t \\r \n world!"];;
test_sexps_lists "40" (read_sexprs("\"  Hi  \"")) [String "  Hi  "];;
test_sexps_lists "41" (read_sexprs("123abc")) [Symbol "123abc"];;
test_sexps_lists "42" (read_sexprs("123AbC")) [Symbol "123abc"];;
test_sexps_lists "43" (read_sexprs("$Dollar")) [Symbol "$dollar"];;
test_sexps_lists "44" (read_sexprs("!$HI")) [Symbol "!$hi"];;

(* test_sexps_lists "45" (read_sexprs("#{x}=1")) [TaggedSexpr ("x", Number (Fraction(1,1)))];;
test_sexps_lists "46" (read_sexprs("#{x}=(a . #{x})")) [TaggedSexpr ("x", Pair (Symbol "a", TagRef "x"))];; *)

test_sexps_lists "47" (read_sexprs("foo (1 2 3) (1 foo 2)")) [Symbol("foo"); Pair(Number(Fraction(1,1)), Pair(Number(Fraction(2,1)), Pair(Number(Fraction(3,1)), Nil))); Pair(Number(Fraction(1,1)), Pair(Symbol("foo"), Pair(Number(Fraction(2,1)), Nil)))];;
test_sexps_lists "48" (read_sexprs("(1 2 3 foo (1 foo))")) [Pair(Number(Fraction(1,1)), Pair(Number(Fraction(2,1)), Pair(Number(Fraction(3,1)), Pair(Symbol("foo"), Pair(Pair(Number(Fraction(1,1)), Pair(Symbol("foo"), Nil)), Nil)))))];;

test_sexps_lists "49" (read_sexprs("foo (2 3 (2 3 (1 foo)))")) [Symbol("foo"); Pair(Number(Fraction(2,1)), Pair(Number(Fraction(3,1)), Pair(Pair(Number(Fraction(2,1)), Pair(Number(Fraction(3,1)), Pair(Pair(Number(Fraction(1,1)), Pair(Symbol("foo"), Nil)), Nil))), Nil)))];;
test_sexps_lists "50" (read_sexprs("; #{Comment}=1 \n 1")) [Number(Fraction(1,1))];;
(* test_sexps_lists "51" (read_sexprs("#{x}=(a . #{x}=3)")) Exception: X_this_should_not_happen.;;
test_sexps_lists "52" (read_sexprs("#{x}=#{x}=3")) Exception: X_this_should_not_happen.;; *)
(* test_sexps_lists "53" (read_sexprs("#{x}=#{y}=3")) [TaggedSexpr ("x", TaggedSexpr ("y", Number (Fraction(3,1))))];; *)
test_sexps_lists "54" (read_sexprs("(define x 1)")) [Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)))];;
test_sexps_lists "55" (read_sexprs("(1 2 (1 . #t))")) [Pair(Number(Fraction(1,1)), Pair(Number(Fraction(2,1)), Pair(Pair(Number(Fraction(1,1)), Bool(true)), Nil)))];;
test_sexps_lists "56" (read_sexprs("(1 2.3 10e-3 #t ,#F ,@(a . 36) b);comment")) [Pair (Number (Fraction(1,1)),Pair (Number (Float 2.3),Pair (Number (Float 0.01), Pair (Bool true,Pair (Pair (Symbol "unquote", Pair (Bool false, Nil)), Pair(Pair (Symbol "unquote-splicing",Pair (Pair (Symbol "a", Number (Fraction(36,1))), Nil)),Pair (Symbol("b"), Nil)))))))];;

test_sexps_lists "57" (read_sexprs("(#; 1 (2 #; 3) #; (1 2) 3 #; #; (1 #; 3) 2 3)")) [Pair (Pair (Number (Fraction(2,1)), Nil),Pair (Number (Fraction(3,1)), Pair (Number (Fraction(3,1)), Nil)))];;
test_sexps_lists "58" (read_sexprs("'a")) [Pair (Symbol "quote", Pair (Symbol "a", Nil))];;
test_sexps_lists "59" (read_sexprs("`(1 ;asd\n 2 3 #;#;#;123 2 3)")) [Pair (Symbol "quasiquote",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Nil))];;
test_sexps_lists "60" (read_sexprs(",\"string\"")) [Pair (Symbol "unquote", Pair (String "string", Nil))];;
test_sexps_lists "61" (read_sexprs(",@123")) [Pair (Symbol "unquote-splicing", Pair (Number (Fraction(123,1)), Nil))];;
test_sexps_lists "62" (read_sexprs("(a . (b #;#t . ( (c . d) . e)))")) [Pair (Symbol "a",Pair (Symbol "b", Pair (Pair (Symbol "c", Symbol "d"), Symbol "e")))];;
test_sexps_lists "63" (read_sexprs("((a b c) . (d . ( (e . f) . g)))")) [Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))),Pair (Symbol "d", Pair (Pair (Symbol "e", Symbol "f"), Symbol "g")))];;
test_sexps_lists "64" (read_sexprs("(a b 1 (c . d) e . ())")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction(1,1)),Pair (Pair (Symbol "c", Symbol "d"), Pair (Symbol "e", Nil)))))];;
test_sexps_lists "65" (read_sexprs("(a b 1 (() . d) e . ())")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction(1,1)),Pair (Pair (Nil, Symbol "d"), Pair (Symbol "e", Nil)))))];;
test_sexps_lists "66" (read_sexprs("(().())")) [Pair (Nil, Nil)];;
test_sexps_lists "67" (read_sexprs("123456789e-9")) [Number (Float 0.123456789)];;
test_sexps_lists "68" (read_sexprs("1.23e+1")) [Number (Float 12.3)];;
(* test_sexps_lists "69" (read_sexprs("#31RU")) [Number (Fraction(3,1)0)];;
test_sexps_lists "70" (read_sexprs("#12R0")) [Number (Fraction(0,1))];;
test_sexps_lists "71" (read_sexprs("#13R15")) [Number (Fraction(1,1)8)];; *)
test_sexps_lists "72" (read_sexprs("\"This is a very long\nstring that spills across\nseveral lines.\"")) [String "This is a very long\nstring that spills across\nseveral lines."];;

Printf.printf "\nAll Done!\n";;
