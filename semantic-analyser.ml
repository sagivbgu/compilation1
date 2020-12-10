#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;
exception X_debug;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

let rec annotate_lex_addr e = annotate_lex_addr_expr [] e

(* Order: varlist e is important for currying in List.Map *)
and annotate_lex_addr_expr varlist e = match e with
| Const(expr) -> Const'(expr)
| Var(name) -> annot_lex_addr_var name varlist
| If(test, dit, dif) -> annot_lex_addr_if test dit dif varlist
| Seq(expr_list) -> annot_lex_addr_seq expr_list varlist
| Set(lhs, rhs) -> raise X_not_yet_implemented
| Def(lhs, rhs) -> annotate_lex_addr_def lhs rhs varlist
| Or(expr_list) -> annotate_lex_addr_or expr_list varlist
| LambdaSimple(params, body) -> annotate_lex_addr_lambda_simple params body varlist
| LambdaOpt(params_list, arg_opt, body) -> raise X_not_yet_implemented
| Applic(func_expr, args_list) -> raise X_not_yet_implemented

and annot_lex_addr_var name varlist = match varlist with
| [] -> Var'(VarFree(name))
| VarParam(name_to_comp, index)::rest -> (if (String.equal name name_to_comp) 
                                  then (Var'(VarParam(name, index)))
                                  else ( (annot_lex_addr_var name rest) ))
| VarBound(name_to_comp, major, minor)::rest -> (if (String.equal name name_to_comp) 
                                  then (Var'(VarBound(name, major, minor)))
                                  else ( (annot_lex_addr_var name rest) ))
| _ -> raise X_debug

and annot_lex_addr_if test dit dif varlist = 
  If'((annotate_lex_addr_expr varlist test), 
      (annotate_lex_addr_expr varlist dit),
      (annotate_lex_addr_expr varlist dif))

and annot_lex_addr_seq expr_list varlist = 
  let expr'_list = List.map (annotate_lex_addr_expr varlist) expr_list in
  Seq'(expr'_list)

and annotate_lex_addr_or expr_list varlist =
  let expr'_list = List.map (annotate_lex_addr_expr varlist) expr_list in
  Or'(expr'_list)

and annotate_lex_addr_def lhs rhs varlist = match lhs with
| Var(str) -> Def'(VarFree(str) , (annotate_lex_addr_expr varlist rhs))
| _ -> raise X_syntax_error

and annotate_lex_addr_lambda_simple params body varlist = 
  let varlist = List.map (update_var_in_varlist params) varlist in
  let new_varlist = List.fold_left (add_param_to_varlist params) varlist params in
  (LambdaSimple'(params, (annotate_lex_addr_expr new_varlist body)))

and is_param_missing param varlist = match varlist with
| [] -> true
| VarParam(name, index)::tail -> (String.equal param name) || (is_param_missing param tail)
| _::tail -> (is_param_missing param tail)

and add_param_to_varlist params varlist name = 
  if (is_param_missing name varlist)
  then (VarParam(name, (get_param_index name params))::varlist)
  else (varlist)

(* call on varlist when entering an embedded scope
  The function "shifts" all the vars to the next level:
    param,index -> bound,0,index
    bound,maj,nin -> bound,(maj+1),min 
    
  If encoutered with a parameter - change the var into Param(name, new_index)
    *)
and update_var_in_varlist params var = match var with
| VarParam(name, index) -> 
  (if (List.mem name params)
    then (VarParam(name, (get_param_index name params)))
    else (VarBound(name, 0, index))
  )
| VarBound(name, major, minor) -> 
  (if (List.mem name params)
    then (VarParam(name, (get_param_index name params)))
    else (VarBound(name, (major + 1), minor))
  )
| _ -> raise X_debug

and get_param_index param lst = match lst with 
  (* To avoid this - wrap this function with "if List.mem ... " *)
  | [] -> -1 
  | head::tail -> (if (String.equal param head) 
                  then (0)
                  else (1 + (get_param_index param tail)))

let annotate_lexical_addresses e = annotate_lex_addr e;;


let annotate_tail_calls e = raise X_not_yet_implemented;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)

open Reader;;
open Tag_Parser;;
open Semantics;;
