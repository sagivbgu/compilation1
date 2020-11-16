#use "quote.ml";;

(* 
  ⟨Char⟩ ::= ⟨CharPrefix⟩ ( ⟨VisibleSimpleChar⟩ | ⟨NamedChar⟩)
  ⟨CharPrefix⟩ ::= #\
  ⟨VisibleSimpleChar⟩ ::= c, where c is a character that is greater than
                         the space character in the ASCII table
  ⟨NamedChar⟩ ::= newline, nul, page, return, space, tab
*)

let char_prefix = word "#\\";;

let make_named_char nc ascii_num = 
  let tok = caten char_prefix (word_ci nc) in
  let tok = make_netto tok in
  let handle_named_char = (function _ -> Char (char_of_int ascii_num)) in
  pack tok handle_named_char;;

let tok_named_char = 
  let nul = make_named_char "nul" 0 in
  let newline = make_named_char "newline" 10 in
  let return = make_named_char "return" 13 in
  let tab = make_named_char "tab" 9 in
  let formfeed = make_named_char "page" 12 in
  let space = make_named_char "space" 32 in
  let named_chars = disj_list [nul; newline; return; tab; formfeed; space] in
  named_chars;;

let tok_visiable_char = 
  let vis_char = range (char_of_int 33) (char_of_int 255) in 
  let vis_char = caten char_prefix vis_char in
  let vis_char = make_netto vis_char in
  let handle_vis_char = (function (_,c) -> Char c) in
  pack vis_char handle_vis_char;;

let nt_char = 
  disj tok_named_char tok_visiable_char;;

(* TESTS *)
Printf.printf("\nchar tests\n");;

nt_char (string_to_list " #\\newline asb");;
nt_char (string_to_list " #\\nul asb");;
nt_char (string_to_list " #\\tab asb");;
nt_char (string_to_list " #\\PAGE asb");;
nt_char (string_to_list " #\\space asb");;
nt_char (string_to_list " #\\RETURN asb");;
nt_char (string_to_list " #\\r eturn asb");;
nt_char (string_to_list " #\\s pace asb");;
nt_char (string_to_list " #\\p age asb");;
nt_char (string_to_list " #\\t ab asb");;
nt_char (string_to_list " #\\n ul asb");;
nt_char (string_to_list " #\\2 ul asb");;
