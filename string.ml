#use "quote.ml";;

(*
  ⟨String⟩ ::= " ⟨StringChar⟩* "
  ⟨StringChar⟩ ::= ⟨StringLiteralChar⟩ | ⟨StringMetaChar⟩
  ⟨StringLiteralChar⟩ ::= c, where c is any character other than the
    backslash character or the double-quote char
  ⟨StringMetaChar⟩ ::= " \\ " | " \" " | " \t " | " \f " | " \n " | " \r "
*)

(*
    Value          Scheme string       The input to the reader
   return               /r                        //r
   newline              /n                        //n
    tab                 /t                        //t
    page                /f                        /f
  backslash             //                        ////
double quote            /"                        ///"
             (Slashes here represent backslashes)
*)

let char_double_quote = char '"'
let char_backslash = char '\\'
let char_t = char_ci 't';;
let char_f = char_ci 'f';;
let char_n = char_ci 'n';;
let char_r = char_ci 'r';;

let escaped c = caten char_backslash c;;

let make_meta_char nt_char value =
  let escaped_nt_char = escaped nt_char in
  pack escaped_nt_char (fun _ -> (value));;

let string_meta_char = 
  let r = make_meta_char char_r '\r' in
  let n = make_meta_char char_n '\n' in
  let t = make_meta_char char_t '\t' in
  let f = make_meta_char char_f '\012' in
  let bs = make_meta_char char_backslash '\\' in
  let dq = make_meta_char char_double_quote '"' in
  disj_list [r ; n ; t ; f ; bs ; dq];;

let string_literal_char =
  let not_double_quote = diff nt_any char_double_quote in
  diff not_double_quote char_backslash;;

let string_char = disj string_meta_char string_literal_char;;
let nt_string =
  let nt = make_paired char_double_quote (star string_char) char_double_quote in
  pack nt (fun e -> String((list_to_string e)));;


(* === Tests === *)
Printf.printf("\nTests: string.ml\n");;

let test_nt_string str expected_match expected_rest_as_string =
  let (String(mat), rest) = nt_string (string_to_list str) in
  assert (mat = expected_match && rest = (string_to_list expected_rest_as_string));;

let test_nt_string_failure str =
try nt_string (string_to_list str);
    assert false
with X_no_match -> true;;

test_nt_string "\"123a\"" "123a" "";;
test_nt_string "\"a\\fb\\nc\\nd\"" "a\012b\nc\nd" "";;
test_nt_string "\"\na\fb\\nc\\nd\t\"" "\na\012b\nc\nd\t" "";;
test_nt_string "\"\\\\ \\\" \\r\\n\\t\\f\"" "\\ \" \r\n\t\012" "";;
test_nt_string "\"\r\"" "\r" "";;
test_nt_string "\"\\\\f\"" "\\f" "";;
test_nt_string "\"f\"" "f" "";;

test_nt_string "\"A\\FB\\NC\\ND\"" "A\012B\nC\nD" "";;
test_nt_string "\"\NA\FB\\NC\\ND\T\"" "\nA\012B\nC\nD\t" "";;
test_nt_string "\"\\\\ \\\" \\R\\N\\T\\F\"" "\\ \" \r\n\t\012" "";;
test_nt_string "\"\R\"" "\r" "";;
test_nt_string "\"\\\\F\"" "\\F" "";;
test_nt_string "\"F\"" "F" "";;

test_nt_string "\"afB\"" "afB" "";;
test_nt_string "\"(afB)\"" "(afB)" "";;
test_nt_string "\"A\"QQ" "A" "QQ";;
test_nt_string "\"a\"z\"\"" "a" "z\"\"";;

test_nt_string_failure "a";;
test_nt_string_failure "a\"b\"";;
test_nt_string_failure "\"a";;
test_nt_string_failure "a\"";;
test_nt_string_failure "\"\\\"";;
test_nt_string_failure "\"a\\z\"";;
test_nt_string_failure "\\\"a\"z\"";;
