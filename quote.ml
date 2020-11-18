#use "number.ml";;
(*
  ⟨Quoted⟩ ::= '⟨Sexpr⟩
  ⟨QuasiQuoted⟩ ::= `⟨Sexpr⟩
  ⟨Unquoted⟩ ::= ,⟨Sexpr⟩
  ⟨UnquoteAndSpliced⟩ ::= ,@⟨Sexpr⟩
*)

let make_nt_quote nt_sexpr = 
  let _Q = '\'' in
  let _QQ = '`' in
  let _UNQ = ',' in
  let _UNSP = ",@" in

  let _packer name = (function (_, exp) -> Pair ((Symbol name), (Pair (exp, Nil)))) in

  let _q = caten (char _Q) nt_sexpr in
  let _q = pack _q (_packer "quote") in

  let _qq = caten (char _QQ) nt_sexpr in
  let _qq = pack _qq (_packer "quasiquote") in

  let _unq = caten (char _UNQ) nt_sexpr in
  let _unq = pack _unq (_packer "unquote") in

  let _unsp = caten (word _UNSP) nt_sexpr in
  let _unsp = pack _unsp (_packer "unquote-splicing") in

  disj_list [_q ; _qq ; _unq ; _unsp];;
  
(* TESTS *)
Printf.printf("\nquote tests\n");;

nt_quote (string_to_list " '7 ");;
nt_quote (string_to_list " `6 ");;
nt_quote (string_to_list " ,5 ");;
nt_quote (string_to_list " ,@4 ");;