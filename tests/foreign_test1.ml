
#use "reader.ml";;
open Reader;;

let eq sexp_list1 sexp_list2 = 
  let s1 = List.hd sexp_list1 in
  let s2 = List.hd sexp_list2 in
  sexpr_eq s1 s2;;

let test_exp res expected = if res = [] then false else 
  if eq res expected then true else false;;

let int1 = test_exp (read_sexprs("1")) ([Number(Fraction(1,1))]);;
let int2 = test_exp (read_sexprs("  1")) ([Number(Fraction(1,1))]);;
let int3 = test_exp (read_sexprs("  1 \t ")) ([Number(Fraction(1,1))]);;
let int4 = test_exp (read_sexprs("01234")) ([Number(Fraction(1234,1))]);;
let int5 = test_exp (read_sexprs("001234")) ([Number(Fraction(1234,1))]);;
let int6 = test_exp (read_sexprs("-01234")) ([Number(Fraction(-1234,1))]);;
let int7 = test_exp (read_sexprs("+01234")) ([Number(Fraction(1234,1))]);;
let int8 = test_exp (read_sexprs("+00940")) ([Number(Fraction(940,1))]);;
let frac1 = test_exp (read_sexprs("1/1")) ([Number(Fraction(1,1))]);;
let frac2 = test_exp (read_sexprs("2/4")) ([Number(Fraction(1,2))]);;
let frac3 = test_exp (read_sexprs("-17/6")) ([Number(Fraction(-17,6))]);;
let frac4 = test_exp (read_sexprs("+006/012")) ([Number(Fraction(1,2))]);;

let sci1 = test_exp (read_sexprs("+1e1")) ([Number(Float(10.0))]);;
let sci2 = test_exp (read_sexprs("1E+1")) ([Number(Float(10.0))]);;
let sci3 = test_exp (read_sexprs("3.14e-1")) ([Number(Float(0.314))]);;
let sci4 = test_exp (read_sexprs("  \n 3.14e-1\t\t\t")) ([Number(Float(0.314))]);;
let sci5 = test_exp (read_sexprs("+0000012.3E00000000002")) ([Number(Float(1230.0))]);;
let sci6 = test_exp (read_sexprs("-5.000000e-2")) ([Number(Float(-0.05))]);;
let sci7 = test_exp (read_sexprs("+5.000000e1")) ([Number(Float(50.0))]);;
let sci8 = test_exp (read_sexprs(";testing a <>?<>?: comment\n+5.000000e1;comment!!")) ([Number(Float(50.0))]);;

let float1 = test_exp (read_sexprs("1.0")) ([Number(Float(1.0))]);;
let float2 = test_exp (read_sexprs("    1.0     ")) ([Number(Float(1.0))]);;
let float3 = test_exp (read_sexprs("  005.01290     ")) ([Number(Float(5.0129))]);;
let float4 = test_exp (read_sexprs("  501.000000     ")) ([Number(Float(501.0))]);;
let float5 = test_exp (read_sexprs(" +999.009000     ")) ([Number(Float(999.009))]);;
let float6 = test_exp (read_sexprs(" -001.000123000     ")) ([Number(Float(-1.000123))]);;

let bool1 = test_exp (read_sexprs(" #t  ")) ([Bool(true)]);;
let bool2 = test_exp (read_sexprs(" \012 #T  ")) ([Bool(true)]);;
let bool3 = test_exp (read_sexprs("#f  ")) ([Bool(false)]);;
let bool4 = test_exp (read_sexprs("\n#F  ")) ([Bool(false)]);;
let bool5 = test_exp (read_sexprs("\n#F  ")) ([Bool(false)]);;
let bool6 = test_exp (read_sexprs("\n#t  #t  ")) ([Bool(true); Bool(true)]);;

let symbol1 = test_exp (read_sexprs(" 1a^  ")) ([Symbol("1a^")]);;
let symbol2 = test_exp (read_sexprs(" 1a^<:  ")) ([Symbol("1a^<:")]);;
let symbol3 = test_exp (read_sexprs("AbC")) ([Symbol("abc")]);;
let symbol4 = test_exp (read_sexprs("a1+3====1.1")) ([Symbol("a1+3====1.1")]);;
let symbol5 = test_exp (read_sexprs("..")) ([Symbol("..")]);;
let symbol6 = test_exp (read_sexprs("..123Ac^;comment")) ([Symbol("..123ac^")]);;

let string1 = test_exp (read_sexprs("\"\"")) ([String("")]);;
let string2 = test_exp (read_sexprs("\"    \"")) ([String("    ")]);;
let string3 = test_exp (read_sexprs("   \"\"    ")) ([String("")]);;
let string4 = test_exp (read_sexprs("\"hello . ^ . \"")) ([String("hello . ^ . ")]);;
let string5 = test_exp (read_sexprs("\"hello . ^ [] ; a {} #@\"")) ([String("hello . ^ [] ; a {} #@")]);;
let string6 = test_exp (read_sexprs("\"\\r\"")) ([String("\r")]);;
let string7 = test_exp (read_sexprs("\"\\n\"")) ([String("\n")]);;
let string8 = test_exp (read_sexprs("\"\\t\"")) ([String("\t")]);;
let string9 = test_exp (read_sexprs("\"\\f\"")) ([String("\012")]);;
let string10 = test_exp (read_sexprs("\"\\\\\"")) ([String("\\")]);;

let char1 = test_exp (read_sexprs("#\\a")) ([Char('a')]);;
let char2 = test_exp (read_sexprs("   #\\a\t")) ([Char('a')]);;
let char3 = test_exp (read_sexprs("   #\\A\t")) ([Char('A')]);;
let char4 = test_exp (read_sexprs("   #\\nul")) ([Char(char_of_int 0)]);;
let char5 = test_exp (read_sexprs("   #\\newline")) ([Char(char_of_int 10)]);;
let char6 = test_exp (read_sexprs("   #\\return")) ([Char(char_of_int 13)]);;
let char7 = test_exp (read_sexprs("   #\\tab")) ([Char(char_of_int 9)]);;
let char8 = test_exp (read_sexprs("   #\\page")) ([Char(char_of_int 12)]);;
let char9 = test_exp (read_sexprs("   #\\space")) ([Char(' ')]);;

let quote1 = test_exp (read_sexprs("'1")) ([Pair(Symbol("quote"), Pair(Number(Fraction(1,1)), Nil))]);;
let quote2 = test_exp (read_sexprs("'3+2")) ([Pair(Symbol("quote"), Pair(Symbol("3+2"), Nil))]);;
let quote3 = test_exp (read_sexprs("'(a 1 . a)")) ([Pair(Symbol("quote"), Pair( Pair(Symbol("a"), Pair(Number(Fraction(1,1)), Symbol("a"))) , Nil))]);;
let quote4 = test_exp (read_sexprs("'(a #;1 . a)")) ([Pair(Symbol("quote"), Pair(Pair(Symbol("a"),Symbol("a")) , Nil))]);;
let quote5 = test_exp (read_sexprs("'(a a #;r)")) ([Pair(Symbol("quote"),  Pair(Pair(Symbol("a"),Pair(Symbol"a",Nil)), Nil))]);;

let nested_lists1 = test_exp (read_sexprs("( 1.23e-2 a!^< (sym1 2sym))")) ([Pair(Number(Float(0.0123)),Pair(Symbol("a!^<"), Pair(Pair(Symbol("sym1"),Pair(Symbol("2sym"),Nil)),Nil)))]);;
let nested_lists2 = test_exp (read_sexprs("\t( \"str1\" . \"str2\\\"\" )")) ([Pair(String("str1"),String("str2\""))]);;
let nested_lists3 = test_exp (read_sexprs("(() ())")) ([Pair(Nil,Pair(Nil,Nil))]);;
let dotted_list1 = test_exp (read_sexprs("(1.1 . (1.2 . (1.3 . ())))")) ([Pair(Number(Float (1.1)), Pair(Number(Float(1.2)),Pair(Number(Float(1.3)),Nil)))]);;
let dotted_list2 = test_exp (read_sexprs("(1.1 . (1.2 . (1.3 . ())))")) (read_sexprs("(1.1 1.2 1.3 )"));;
let dotted_list3 = test_exp (read_sexprs("(1.1 #;(1.2 1.3))")) ([Pair(Number(Float (1.1)),Nil)]);;
let empty_list = test_exp (read_sexprs("()")) ([Nil]);;