#use "number.ml";;

(* 
  ⟨Symbol⟩ ::= ⟨SymbolCharNoDot⟩ | ⟨SymbolChar⟩⟨SymbolChar⟩+
  ⟨SymbolChar⟩ ::= ⟨SymbolCharNoDot⟩ | ⟨Dot⟩
  ⟨SymbolCharNoDot⟩ ::= (0 | ... | 9) | (a | ... | z) | (A | ... | Z) | 
                      ! | $ | ^ | * | - | _ | = | + | < | > | ? | / | :
  ⟨Dot⟩ ::= .
 *)

let letter = range_ci 'a' 'z';;
let punctuation = one_of "!$^*-_=+<>/?";;
let dot = char '.';;

let symbol_char_no_dot =
  let digit_or_letter = disj digit letter in
  disj digit_or_letter punctuation;;

let symbol_char = disj symbol_char_no_dot dot

let symbol =
  let symbol_char_plus = plus symbol_char in
  let symbol_chars = caten symbol_char symbol_char_plus in
  let symbol_chars = pack symbol_chars (fun (c, cplus) -> c :: cplus) in
  let symbol_chars = pack symbol_chars list_to_string in
  let symbol_char_no_dot = pack symbol_char_no_dot (function c -> String.make 1 c) in 
  disj symbol_chars symbol_char_no_dot;;

let nt_symbol = pack symbol (function e -> Symbol(e));;


(* === Tests === *)
Printf.printf("\nTests: symbol.ml\n");;

let test_nt_symbol str expected_match expected_rest_as_string =
  let (Symbol(mat), rest) = nt_symbol (string_to_list str) in
  assert (mat = expected_match && rest = (string_to_list expected_rest_as_string));;


let test_nt_symbol_failure str =
try nt_symbol (string_to_list str);
    assert false
with X_no_match -> true;;

test_nt_symbol "123a" "123a" "";;
test_nt_symbol "!$^*-_=+<>/? " "!$^*-_=+<>/?" " ";;
test_nt_symbol ".!$^*-_=+<>/? w1" ".!$^*-_=+<>/?" " w1";;
test_nt_symbol "0.!$^*-_=+<>/? ." "0.!$^*-_=+<>/?" " .";;
test_nt_symbol "a0.!$^*-_=+<>/?@" "a0.!$^*-_=+<>/?" "@";;
test_nt_symbol ".w" ".w" "";;
test_nt_symbol ".w." ".w." "";;

test_nt_symbol_failure "";;
test_nt_symbol_failure " ";;
test_nt_symbol_failure ".";;
test_nt_symbol_failure "@";;
test_nt_symbol_failure ". a";;

exception X_make_netto_not_implemented_yet;;
raise X_make_netto_not_implemented_yet;;