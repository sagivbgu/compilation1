
#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type number =
  | Fraction of int * int
  | Float of float;;

type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
  let normalize_scheme_symbol str =
    let s = string_to_list str in
    if (andmap
          (fun ch -> (ch = (lowercase_ascii ch)))
          s) then str
    else Printf.sprintf "|%s|" str;;

  (* ***************** UTILS ***************** *)

  let make_paired nt_left nt nt_right=
    let nt = caten nt_left nt in
    let nt = pack nt (function (_, e) -> e) in
    let nt = caten nt nt_right in
    let nt = pack nt (function (e, _) -> e) in
    nt;;

  (*
  Line comments start with the semicolon character ; and continue
  until either an end-of-line or end-of-input is reached.
  The semicolon may appear anywhere on the line, and need not be the first character.
  *)

  let nt_semicolon = char ';';;

  let nt_line_comment = 
    let nt_end_of_line = char '\n' in
    let nt_end_of_line = pack nt_end_of_line (fun _ -> []) in
    let nt_end_line_comment = disj nt_end_of_line nt_end_of_input in
    let nt_any = diff nt_any nt_end_line_comment in
    let nt_any_star = star nt_any in
    let nt = make_paired nt_semicolon nt_any_star nt_end_line_comment in
    pack nt (fun _ -> []);;

  let nt_space =
    let nt_whitespace = pack nt_whitespace (fun _ -> []) in
    disj nt_whitespace nt_line_comment;;
  let nt_space_star = star nt_space;;

  let make_spaced nt =
    make_paired nt_space_star nt nt_space_star;;

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
    

  (* ***************** BOOLEAN ***************** *)

  (* Parser for #[t,T,f,F] *)
  let tok_bool = 
    let _t = char_ci 't' in
    let _f = char_ci 'f' in 
    let _t_or_f = disj _t _f in
    let _sign = char '#' in 
    caten _sign _t_or_f;;

  let nt_bool = 
    let _create_ast = (function (_, b) -> match b with
        | 'f' -> Bool false
        | 'F' -> Bool false
        | 't' -> Bool true
        | 'T' -> Bool true
        | _ -> raise X_this_should_not_happen) in 
    pack tok_bool _create_ast;;

  (* ***************** CHAR ***************** *)

  let char_prefix = word "#\\";;

  let make_named_char nc ascii_num = 
    let tok = caten char_prefix (word_ci nc) in
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
    let handle_vis_char = (function (_,c) -> Char c) in
    pack vis_char handle_vis_char;;

  let nt_char = 
    disj tok_named_char tok_visiable_char;;

  (* ***************** LIST ***************** *)

  let make_nt_parenthesized_expr nt s =
    (* Printf.printf "In make_nt_parenthesized_expr\n"; 
    Printf.printf "\tparamt s= |%s|\n" (list_to_string s); *)
    make_paired (char '(') nt (char ')');; 

  let tok_list nt_sexpr s = make_nt_parenthesized_expr (star nt_sexpr) s;;

  let tok_dotted_list nt_sexpr s = 
    let dot = char '.' in
    let p_sexpr = (plus nt_sexpr) in
    let dotted_sexpr = caten p_sexpr (caten dot nt_sexpr) in
    let parenth_dotted_sexpr = make_nt_parenthesized_expr dotted_sexpr s in
    let _remove_dot = 
      (function (l, (d, r)) -> l@[r]) in
    pack parenth_dotted_sexpr _remove_dot;;

  let rec list_to_pairs_end_with_nil lst = 
    (* Printf.printf "In list_to_pairs_end_with_nil:\n";  *)
    match lst with
    | [] -> Nil
    | a::rest -> 
    (* Printf.printf "\tparam a: %s\n" (unread a); *)
    Pair (a,(list_to_pairs_end_with_nil rest));;

  let rec list_to_pairs lst = 
    match lst with
    | a::[b] -> Pair (a, b)
    | a::rest -> Pair (a,(list_to_pairs rest));;

  let make_nt_list nt_sexpr s =
    pack (tok_list nt_sexpr s) list_to_pairs_end_with_nil;;

  let make_nt_dotted_list nt_sexpr s = 
    pack (tok_dotted_list nt_sexpr s) list_to_pairs;;


  (* ***************** NUMBER ***************** *)

  (*** DIGITS: parsers and methods hanlding digits ***)
  let digit = range '0' '9';;
  let digits = plus digit;;

  (* convert a char of a digit to its actual number value *)
  let digit_char_to_digit_num d = 
    (int_of_char(d) - 48);;

  (* convert a list of digits chars to a list of the actual digits (as numbers) *)
  let digits_to_nums = (function lst -> 
      (List.map digit_char_to_digit_num lst));;

  (* convert a list of digits chars to a list of the actual digits (as floats) *)
  let digits_to_floats lst = 
    let digits_as_nums = (digits_to_nums lst) in
    (List.map Float.of_int digits_as_nums);;

  (* NATURAL: parsers and methods handling natural numbers *)
  (* a parser for natural numbers *)
  let natural = digits;;

  (* a packer for natural numbers for integers (ignoring left zeros)*)
  let digits_lst_to_integer = 
    let acc = (fun a b -> 10 * a + b) in
    (function ds_lst -> 
       (List.fold_left acc 0 ds_lst));;

  (* a packer for natural numbers for mantissas (ignoring right zeros) *)
  let digits_lst_to_mantissa = 
    let acc = (fun a b -> (a +. b) /. 10.0) in
    (function ds_lst ->
       (List.fold_right acc ds_lst 0.0));;

  (* a packed parser for natural number for integers 
     (as differed from natural number for mantissa) *)
  let nat_as_integer = 
    let natural_to_digits_lst = pack natural digits_to_nums in
    pack natural_to_digits_lst digits_lst_to_integer;;

  (* SIGN: a parser of sign [+,-]*)
  let sign_plus_or_minus = 
    let _plus = char '+' in
    let _minus = char '-' in
    let _sign = disj _plus _minus in
    _sign;;

  (* packing function for maybe sign, converting + to 1 and - to -1 *)
  let sign_to_num = (function s -> match s with
      | Some('-') -> -1
      | _ -> 1
    );;


  (* INTEGER: a parser for integers (sign)?[natural as integer] *)
  let tok_integer = 
    let _nat = nat_as_integer in
    let _sign = maybe sign_plus_or_minus in
    let _sign = pack _sign sign_to_num in
    let _int = caten _sign _nat in
    let _packer = (function (s,nat) -> s * nat) in
    let _signed_int = pack _int _packer in
    _signed_int;;


  (* MANTISSA: *)
  (* a packed parser for natural number for the mantissa of floats *) 
  let nat_as_mantissa = 
    let natural_to_digits_lst = pack natural digits_to_floats in
    pack natural_to_digits_lst digits_lst_to_mantissa;;

  (* FLOAT: *)
  (* a parser for floats [integer][.][natural as mantissa] *)
  let tok_float = 
    let _int = tok_integer in
    let _dot = char '.' in
    let _mantissa = nat_as_mantissa in 
    let _float = caten _int (caten _dot _mantissa) in
    let _handle_float = (function (nat,(dot,man)) -> 
        if (nat < 0)
        then (Float.of_int nat) -. man
        else (Float.of_int nat) +. man) in
    pack _float _handle_float;;

  (* FRACTIONS: *)
  (* a function calculating the gcd of two integers *)
  let rec gcd a b =
    if (b = 0) then abs(a) else gcd b (a mod b);;

  (* a parser for fractions [interger][/][natural as integer] *)
  let tok_fraction = 
    let _numerator = tok_integer in
    let _denominator = nat_as_integer in
    let _div = char '/' in
    let _frac = caten _numerator (caten _div _denominator) in
    let _handle_fract = (function (n,(_,d)) -> 
        let _gcd = gcd n d in
        (n / _gcd, d / _gcd)) in
    pack _frac _handle_fract;;

  (* SCIENTIFIC NOTATION *)
  (* a parser for the scientific notation suffix [e,E][integer] *)
  let tok_scientific_suffix = 
    let _e = char_ci 'e' in
    let _int = tok_integer in
    caten _e _int;;

  (* a packer function to create a float out of scientific notation *)
  let handle_scientific_notation = 
    (function (fl, (e, exp)) -> 
       let _exp = Float.of_int exp in
       let _exp = 10. ** _exp in
       fl *. _exp)

  (* a parser for scientific notation preceeding by int *)
  (* here we don't use "maybe tok_scientific_suffix", becuse the return function 
      will have two return types - 
      if it has scientific notation, it's supposed to be float
      if it doesn't, it's supposed to be int
      Ocaml doesn't let this happen *)
  let tok_scientific_int = 
    let _suffix = tok_scientific_suffix in
    let _int = tok_integer in
    let _scien_int = caten _int _suffix in
    let _handle_scien_int = 
      (function (n, (e, exp)) -> handle_scientific_notation (Float.of_int n,(e, exp))) in
    pack _scien_int _handle_scien_int;;

  (* a parser for scientific notation preceeding by float *)
  let tok_scientific_float = 
    let _suffix = maybe tok_scientific_suffix in
    let _float = tok_float in
    let _scien_float = caten _float _suffix in
    let _handle_scien_float = 
      (function (fl, x) -> match x with 
          | Some(e, exp) -> handle_scientific_notation (fl, (e, exp)) 
          | None -> fl
      ) in
    pack _scien_float _handle_scien_float;;

  let tok_scientific = 
    disj tok_scientific_float tok_scientific_int;;

  (* ASTs *)
  (* Integer is Fraction with denominator of 1 *)
  let tok_integer_to_ast =
    let _create_ast = (function n -> Fraction (n,1)) in
    pack tok_integer _create_ast;;

  let tok_float_to_ast = 
    let _create_ast = (function f -> Float f) in
    pack tok_float _create_ast;;

  let tok_fraction_to_ast = 
    let _create_ast = (function (n,d) -> Fraction (n,d)) in
    pack tok_fraction _create_ast;;

  (* Scientific notation is always Float *)
  let tok_scientific_to_ast = 
    let _create_ast = (function f-> Float f) in
    pack tok_scientific _create_ast;;

  (* ⟨Number⟩ ::= ⟨Integer⟩ | ⟨Float⟩ | ⟨Fraction⟩   *)
  (* But in different order : Scientific (Float or Int) | Fraction | Integer, 
     so Integer won't catch everything, and Float won't catch Scientific*)
  let nt_number = 
    let _number = disj tok_scientific_to_ast (* Number(Float (X.Y)) *)
        (disj tok_fraction_to_ast (* Number(Fraction (X/Y)) *)
           tok_integer_to_ast) (* Number(Fraction (X/1)) *) in
    pack _number (function n -> Number n);;

  (* ***************** QUOTE ***************** *)

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

  (* ***************** SYMBOL ***************** *)

  let letter = 
    let l = range_ci 'a' 'z' in
    pack l (fun c -> lowercase_ascii c);;
  let punctuation = one_of "!$^*-_=+<>/?:";;
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

  (* TODO: Consider refactoring *)
  let nt_symbol = pack symbol (function e ->
      let rest_empty = function (first, rest) -> rest = [] in
      let return_first = function (first, rest) -> first in
      try
        let nt_number_result = nt_number (string_to_list e) in
        if (rest_empty nt_number_result)
        then (return_first nt_number_result)
        else (raise X_no_match)
      with X_no_match -> Symbol(e));;

  (* ***************** STRING ***************** *)

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

  (* ***************** SEXPR ***************** *)

  let rec nt_sexpr s =
    let sexpr = disj_list [nt_bool; nt_char; nt_symbol; nt_number; nt_string; nt_list;
                           nt_dotted_list; nt_quote] in

    let spaced_sexpr s = (make_spaced sexpr) s in
    let m_comment = maybe nt_sexpr_comment in
    let commented_spaced_sexpr s = (caten m_comment (caten spaced_sexpr m_comment)) s in
    let handle_comments = (function (x,(exp,y)) -> match x with
                                      | Some(x) -> Printf.printf "x=( %s )\n" (unread x); exp
                                      | None -> exp
                                      ) in
    (pack commented_spaced_sexpr handle_comments) s
  
  and nt_list s = (make_nt_list nt_sexpr s) s
  and nt_dotted_list s = (make_nt_dotted_list nt_sexpr s) s
  and nt_quote s = (make_nt_quote nt_sexpr) s

  and nt_sexpr_comment s = 
    let sexpr_comment_start = word "#;" in
    Printf.printf "In nt_sexpr_comment: param s = %s\n" (list_to_string s); 
    (* caten_list must be provided with nts of the same type, so we make comment 
        to a String sexpr *)
    let sexpr_comment_start = pack sexpr_comment_start (fun s -> String(list_to_string s)) in
    let sexpr_comment = caten sexpr_comment_start nt_sexpr in
    let packer = (function (comment,exp) -> Printf.printf "commenting out: |%s|\n" (unread exp); exp) in
    (pack sexpr_comment packer) s

  (* *************** READER ***************** *)
  let read_sexprs string = 
    let first = function (f, s) -> f in
    let nt_sexprs = (star nt_sexpr) (string_to_list string) in
    first nt_sexprs;;

end;; (* struct Reader *)
