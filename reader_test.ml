#use "symbol.ml";;
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
      | [] -> raise TestFail_Result_Ended_Before_Expected
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
test_sexps_lists "Scientific#6" (read_sexprs("+000000012.3E00000002")) ([Number(Float(1230))]);;
test_sexps_lists "Scientific#7" (read_sexprs("3.14E-512")) ([Number(Float(0.))]);;
test_sexps_lists "Scientific#8" (read_sexprs("5e-2")) ([Number(Float(0.05))]);;
test_sexps_lists "Scientific#9" (read_sexprs("      5e-2      ")) ([Number(Float(0.05))]);;

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
(*test_sexps_lists "Symbol#11" (read_sexprs("a0.!$^*-_=+<>/?@")) "a0.!$^*-_=+<>/?" "@";;*)

test_sexps_lists "Number_Before_Symbol#1" (read_sexprs("      5e -2      ")) ([Symbol("5e"); Number(Fraction(-2,1))]);;
test_sexps_lists "Number_Before_Symbol#1" (read_sexprs(" 5 e -2 ")) ([Number(Fraction(5,1); Symbol("e"); Number(Fraction(-2,1))]);;

test_sexps_lists "Comment#1" (read_sexprs(";testing a <>?<>?: comment\n+5.000000e1;comment!!")) ([Number(Float(50.0))]);;

