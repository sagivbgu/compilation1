#use "utils.ml";;
#use "reader.ml";;

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