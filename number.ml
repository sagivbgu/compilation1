#use "utils.ml";;
#use "reader.ml";;

(*
  ⟨Number⟩ ::= ⟨Integer⟩ | ⟨Float⟩ | ⟨Fraction⟩
  ⟨Float⟩ ::= ⟨Integer⟩ . ⟨Natural⟩
  ⟨Fraction⟩ ::= ⟨Integer⟩ / ⟨Natural⟩
  ⟨Integer⟩ ::= (+ | -)? ⟨Natural⟩
  ⟨Natural⟩ ::= ⟨Digit⟩+
  ⟨Digit⟩ ::= (0 | · · · | 9)
*)

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
  pack _int _packer;;

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

(* FRACTIONS *)
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


(* ASTs *)
(* Integer is Fraction with denominator of 1 *)
let tok_integer_to_ast =
  let _int = make_netto tok_integer in
  let _create_ast = (function n -> Fraction (n,1)) in
  pack _int _create_ast;;

let tok_float_to_ast = 
  let _float = make_netto tok_float in
  let _create_ast = (function f -> Float f) in
  pack _float _create_ast;;

let tok_fraction_to_ast = 
  let _fract = make_netto tok_fraction in
  let _create_ast = (function (n,d) -> Fraction (n,d)) in
  pack _fract _create_ast;;

(* ⟨Number⟩ ::= ⟨Integer⟩ | ⟨Float⟩ | ⟨Fraction⟩   *)
(* But in different order : Float | Fraction | Integer, so Integer won't catch everything *)
let tok_number_ast = 
  let _number = disj tok_float_to_ast (disj tok_fraction_to_ast tok_integer_to_ast) in
  pack _number (function n -> Number n);;


(* === Tests === *)
Printf.printf("\nTests: number.ml\n");;
Printf.printf("\nTests: tok_integer_to_ast\n");;
tok_integer_to_ast (string_to_list "123a");;
tok_integer_to_ast (string_to_list "-123a");;
tok_integer_to_ast (string_to_list "+123a");;
tok_integer_to_ast (string_to_list "+00000123a");;
tok_integer_to_ast (string_to_list "-00000123a");;
tok_integer_to_ast (string_to_list "+12300000a");;
tok_integer_to_ast (string_to_list "-12300000a");;
tok_integer_to_ast (string_to_list "   -12300000   a");;

Printf.printf("\nTests: tok_float_to_ast\n");;
tok_float_to_ast (string_to_list "111.1a");;
tok_float_to_ast (string_to_list "111.100a");;
tok_float_to_ast (string_to_list "111.00100a");;
tok_float_to_ast (string_to_list "-111.00100a");;
tok_float_to_ast (string_to_list "+111.00100a");;
tok_float_to_ast (string_to_list "     +111.00100     a");;

Printf.printf("\nTests: tok_fraction_to_ast\n");;
tok_fraction_to_ast (string_to_list "3/6a");;
tok_fraction_to_ast (string_to_list "-3/60a");;
tok_fraction_to_ast (string_to_list "+3/60a");;
tok_fraction_to_ast (string_to_list "+00003/60a");;
tok_fraction_to_ast (string_to_list "     +00003/60   a    ");;
Printf.printf("\nTests: number_ast\n");;
tok_number_ast (string_to_list "     +00003/60   0    ");;
tok_number_ast (string_to_list "     -111.00100     a");;
tok_number_ast (string_to_list "   -12300000   0");;