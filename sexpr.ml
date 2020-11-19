#use "list.ml";;
open PC;;


(*
⟨Sexpr⟩ ::= ⟨Boolean⟩ | ⟨Char⟩ | ⟨Number⟩ | ⟨String⟩ | ⟨Symbol⟩ | ⟨List⟩ |
            ⟨DottedList⟩ | ⟨Quoted⟩ | ⟨QuasiQuoted⟩ | ⟨Unquoted⟩ | ⟨UnquoteAndSpliced⟩
*)

let rec nt_sexpr =
  let sexpr = disj_list [nt_sexpr_comment; nt_bool; nt_char; nt_number; nt_string; nt_symbol; nt_list;
    nt_dotted_list; nt_quote] in
  make_spaced sexpr

and nt_list = make_nt_list make_nt_sexprs
and nt_dotted_list = make_nt_dotted_list make_nt_sexprs
and nt_quote = make_nt_quote nt_sexpr

and nt_sexpr_comment = 
  let sexpr_comment_start = word "#;" in
  let sexpr_comment = caten_list [sexpr_comment_start; nt_sexpr; nt_sexpr] in
  pack sexpr_comment (function first :: [second :: third] -> third)

(* Useful "kleene" functions may be star, plus, identity *)
and make_nt_sexprs kleene = kleene nt_sexpr;;