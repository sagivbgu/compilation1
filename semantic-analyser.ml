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
  | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
  | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
  | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
  | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
  | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
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

  let is_var_of_name var_name expr = match expr with 
    | VarParam(v, _) -> v = var_name
    | VarBound(v, _, _) -> v = var_name
    | VarFree(v) -> v = var_name;;

  let rec annotate_lex_addr e = annotate_lex_addr_expr [] e

  and annotate_lex_addr_expr varlist e = match e with
    | Const(expr) -> Const'(expr)
    | Var(name) -> annot_lex_addr_var varlist name 
    | If(test, dit, dif) -> annot_lex_addr_if varlist test dit dif 
    | Seq(expr_list) -> annot_lex_addr_seq varlist expr_list 
    | Set(lhs, rhs) -> annot_lex_addr_set varlist lhs rhs
    | Def(lhs, rhs) -> annotate_lex_addr_def varlist lhs rhs 
    | Or(expr_list) -> annotate_lex_addr_or varlist expr_list 
    | LambdaSimple(params, body) -> annotate_lex_addr_lambda_simple varlist params body 
    | LambdaOpt(params, arg_opt, body) -> annotate_lex_addr_lambda_opt varlist params arg_opt body 
    | Applic(func, args) -> annotate_lex_addr_applic varlist func args

  and annot_lex_addr_var varlist name = match varlist with
    | [] -> Var'(VarFree(name))
    | VarParam(name_to_comp, index)::rest -> (if (String.equal name name_to_comp) 
                                              then (Var'(VarParam(name, index)))
                                              else ( (annot_lex_addr_var rest name) ))
    | VarBound(name_to_comp, major, minor)::rest -> (if (String.equal name name_to_comp) 
                                                     then (Var'(VarBound(name, major, minor)))
                                                     else ( (annot_lex_addr_var rest name) ))
    | _ -> raise X_this_should_not_happen

  and annot_lex_addr_if varlist test dit dif = 
    If'((annotate_lex_addr_expr varlist test), 
        (annotate_lex_addr_expr varlist dit),
        (annotate_lex_addr_expr varlist dif))

  and annot_lex_addr_seq varlist expr_list = 
    let expr'_list = List.map (annotate_lex_addr_expr varlist) expr_list in
    Seq'(expr'_list)

  (* This weird form is because Set' expects lhs to be of type "var"
     and annot_lex_addr_var returns type expr' ("Var'")
     so we need to extract the "var" out of the "Var'" *)
  and annot_lex_addr_set varlist lhs rhs = match lhs with
    | Var(name) -> 
      (let new_lhs = annot_lex_addr_var varlist name in 
       let set = (function 
           | Var'(var) -> Set'(var, (annotate_lex_addr_expr varlist rhs))
           | _ -> raise X_this_should_not_happen) in
       set new_lhs)
    | _ -> raise X_syntax_error

  and annotate_lex_addr_or varlist expr_list =
    let expr'_list = List.map (annotate_lex_addr_expr varlist) expr_list in
    Or'(expr'_list)

  (* TODO: is define always in outer scope?
          otherwise - we should apply the same logic as set *)
  and annotate_lex_addr_def varlist lhs rhs = match lhs with
    | Var(name) -> Def'(VarFree(name) , (annotate_lex_addr_expr varlist rhs))
    | _ -> raise X_syntax_error

  and annotate_lex_addr_lambda_simple varlist params body = 
    let varlist = List.map (shift_var_to_new_scope params) varlist in
    let new_varlist = List.fold_left (add_param_to_varlist params) varlist params in
    (LambdaSimple'(params, (annotate_lex_addr_expr new_varlist body)))

  and annotate_lex_addr_lambda_opt varlist params arg_opt body =
    let varlist = List.map (shift_var_to_new_scope params) varlist in
    let varlist = List.fold_left (add_param_to_varlist params) varlist params in
    let params_with_opt = params@[arg_opt] in
    let new_varlist = add_param_to_varlist params_with_opt varlist arg_opt in
    (LambdaOpt'(params, arg_opt, (annotate_lex_addr_expr new_varlist body)))

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
  and shift_var_to_new_scope params var = match var with
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
    | _ -> raise X_this_should_not_happen

  and get_param_index param lst = match lst with 
    (* To avoid this case - wrap this function with "if List.mem ... " *)
    | [] -> -1 
    | head::tail -> (if (String.equal param head) 
                     then (0)
                     else (1 + (get_param_index param tail)))

  and annotate_lex_addr_applic varlist func args = 
    let args = List.map (annotate_lex_addr_expr varlist) args in
    Applic'((annotate_lex_addr_expr varlist func), args)

  let rec annotate_tails is_tail expr = match expr with
    | Const'(c) -> Const'(c)
    | Var'(v) -> Var'(v)
    | Box'(v) -> Box'(v)
    | BoxGet'(v) -> BoxGet'(v)
    | BoxSet'(v, e) -> BoxSet'(v, (annotate_tails false e))
    | If'(cond, t, e) -> If'((annotate_tails false cond), (annotate_tails is_tail t), (annotate_tails is_tail e))
    | Seq'(es) -> Seq'(annotate_seq_tails is_tail es)
    | Set'(v, e) -> Set'(v, (annotate_tails false e))
    | Def'(v, e) -> Def'(v, (annotate_tails false e))
    | Or'(es) -> Or'(annotate_seq_tails is_tail es)
    | LambdaSimple'(ss, e) -> LambdaSimple'(ss, (annotate_tails true e))
    | LambdaOpt'(ss, s, e) -> LambdaOpt'(ss, s, (annotate_tails true e))
    | Applic'(e, es) -> 
      if is_tail 
      then ApplicTP'((annotate_tails false e), (List.map (annotate_tails false) es))
      else Applic'((annotate_tails false e), (List.map (annotate_tails false) es))
    | ApplicTP'(e, es) -> ApplicTP'(e, es) (* This expression is already annotated *)

  and annotate_seq_tails is_tail exprs =
    let make_new_list cur acc = match cur, acc, is_tail with
      | Applic'(e, es), [], true -> [ApplicTP'((annotate_tails false e), (List.map (annotate_tails false) es))]
      | cur, [], is_tail -> [(annotate_tails is_tail cur)]
      | cur, acc, _ -> (annotate_tails false cur) :: acc in
    List.fold_right make_new_list exprs [];;

  let annotate_lexical_addresses e = annotate_lex_addr e;;

  let annotate_tail_calls e = annotate_tails false e;;

  let rec annotate_box_set e = box_set_expr e

  and box_set_expr = function
    | Const'(cons) -> raise X_not_yet_implemented
    | Var'(var) -> raise X_not_yet_implemented
    | Box'(var) -> raise X_not_yet_implemented
    | BoxGet'(var) -> raise X_not_yet_implemented
    | BoxSet'(var, rhs) -> raise X_not_yet_implemented
    | If'(test, dit, dif) -> raise X_not_yet_implemented
    | Seq'(exprs) -> raise X_not_yet_implemented
    | Set'(var, rhs) -> raise X_not_yet_implemented
    | Def'(var, rhs) -> raise X_not_yet_implemented
    | Or'(exprs) -> raise X_not_yet_implemented
    | LambdaSimple'(params, body) -> box_set_lambda_simple params body
    | LambdaOpt'(params, opt, body) -> raise X_not_yet_implemented
    | Applic'(func, args) -> raise X_not_yet_implemented
    | ApplicTP'(func, args) -> raise X_not_yet_implemented

  and flatten_applics = function
    | Seq'(exprs) -> Seq'(List.flatten (List.map extract_applic exprs))
    | x -> Seq'((extract_applic x))

  and extract_applic = function
    | Applic'(func, args) -> func::args
    | ApplicTP'(func, args) -> func::args
    | x -> [x]

  and box_set_lambda_simple params body = 
    let params_to_report = params in 
    let flattened_body = flatten_applics body in
    (* TODO: Also flatten Or' *)
    let body_report = (function 
        | Seq'(exprs) -> List.map (report_variables_usage params_to_report) exprs
        | expr -> [(report_variables_usage params_to_report expr)]
      ) in
    let subexps_reports = body_report flattened_body in
    (* 
      In this point we have a list of tuples (reads, writes)
      for each sub expression in the body
      and from here we can apply all logics
    *)

    let is_var_param = function VarParam(_, _) -> true | _ -> false in
    let is_var_bound = function VarBound(_, _, _) -> true | _ -> false in

    let check_need_boxing reports param =
      let list_not_empty = function [] -> false | _ -> true in
      let filter_var_params = List.filter (fun v -> (is_var_of_name param v) && (is_var_param v)) in
      let filter_var_bounds = List.filter (fun v -> (is_var_of_name param v) && (is_var_bound v)) in
      let all_reads = List.map (function (reads, writes) -> reads) reports in
      let reads_var_params = List.map filter_var_params all_reads in
      let reads_var_params_amount = List.length (List.filter list_not_empty reads_var_params) in
      let reads_var_bounds = List.map filter_var_bounds all_reads in
      let reads_var_bounds_amount = List.length (List.filter list_not_empty reads_var_bounds) in
      let all_writes = List.map (function (reads, writes) -> writes) reports in
      let writes_var_params = List.map filter_var_params all_writes in
      let writes_var_params_amount = List.length (List.filter list_not_empty writes_var_params) in
      let writes_var_bounds = List.map filter_var_bounds all_writes in
      let writes_var_bounds_amount = List.length (List.filter list_not_empty writes_var_bounds) in

      let rec same_var_bound_in_different_read_and_write reads writes = 
        let check_read reads all_writes =
          let writes = List.flatten all_writes in
          let same_var_bounds = List.filter (fun var_bound -> List.mem var_bound writes) reads in
          List.length same_var_bounds > 0 in
        match reads, writes with
        | [], [] -> false
        | [], (w :: rest_w) -> false
        | (r :: rest_r), [] -> false
        | (r :: rest_r), (w :: rest_w) -> (check_read r rest_w) || (same_var_bound_in_different_read_and_write rest_r rest_w)
      in

      if (reads_var_params_amount > 0 && writes_var_bounds_amount > 0) ||
         (writes_var_params_amount > 0 && reads_var_bounds_amount > 0)
      then not (is_expr_special_boxing_criteria body param) 
      else if (reads_var_params_amount = 0 || writes_var_params_amount = 0) &&
              reads_var_bounds_amount > 0 &&
              writes_var_bounds_amount > 0 &&
              (same_var_bound_in_different_read_and_write reads_var_bounds writes_var_bounds)
      then true
      else false in
    let vars_to_box = List.filter (check_need_boxing subexps_reports) params_to_report in

    Printf.printf "%s" (print_exps_report subexps_reports);
    Printf.printf "Vars to box: ";
    List.iter (Printf.printf "%s, \n") vars_to_box;

    (* Notice that after this function logic, we should apply again box_set_expr
        on the body *)

    (* temporary return value *)
    LambdaSimple'(params, body)

  and is_expr_special_boxing_criteria expr var_name = match expr with
    | Seq'(exprs) -> is_special_boxing_criteria false false false false exprs var_name
    | _ -> false

  and is_special_boxing_criteria read_occurred write_occurred compound_read_occured compound_write_occurred exprs variable =
    let is_expr_read_occur = function 
      | Var'(VarParam(v, _)) -> v = variable
      | Var'(VarBound(v, _, _)) -> v = variable
      | _ -> false in
    let is_expr_write_occur = function Set'(variable, _) -> true | _ -> false in
    let report_variable_usage expr = (report_variables_usage [variable] expr) in
    let is_expr_deep_read_occur variable_usage_report = (
      let (reads, writes) = variable_usage_report in
      List.length reads > 0) in
    let is_expr_deep_write_occur variable_usage_report = (
      let (reads, writes) = variable_usage_report in
      List.length writes > 0) in
    match read_occurred, write_occurred, compound_read_occured, compound_write_occurred, exprs with
    (* Case: [...; <read-occur>; ...; E<write>; ...] *)
    | true, _, _, true, [] -> true

    (* Case: [...; <read-occur>; ...; E<write>; ...; <read or write-occur> or E<read or write>; ...] *)
    | true, _, _, true, expr :: rest -> (
        let report = report_variable_usage expr in
        if (is_expr_read_occur expr) || (is_expr_deep_read_occur report) ||
           (is_expr_write_occur expr) || (is_expr_deep_write_occur report)
        then false
        else is_special_boxing_criteria read_occurred write_occurred compound_read_occured compound_write_occurred rest variable
      )

    (* Case: [...; <write-occur>; ...; E; ...] *)
    | _, true, true, _, [] -> true

    (* Case: [...; <write-occur>; ...; E<read>; ...; <read or write-occur> or E<read or write>; ...] *)
    | _, true, true, _, expr :: rest -> (
        let report = report_variable_usage expr in
        if (is_expr_read_occur expr) || (is_expr_deep_read_occur report) ||
           (is_expr_write_occur expr) || (is_expr_deep_write_occur report)
        then false
        else is_special_boxing_criteria read_occurred write_occurred compound_read_occured compound_write_occurred rest variable
      )

    (* Case: else *)
    | _, _, _, _, [] -> false

    (* Case: else *)
    | _, _, _, _, expr :: rest -> (
        let report = report_variable_usage expr in
        let read_occurred = (is_expr_read_occur expr) || read_occurred in
        let write_occurred = (is_expr_write_occur expr) || write_occurred in
        let compound_read_occured = (is_expr_deep_read_occur report) || compound_read_occured in
        let compound_write_occurred = (is_expr_deep_write_occur report) || compound_write_occurred in
        is_special_boxing_criteria read_occurred write_occurred compound_read_occured compound_write_occurred rest variable
      )

  and print_var = function
    | VarFree(name) -> Printf.sprintf "VarFree(%s) " name
    | VarParam(name, i) -> Printf.sprintf "VarParam(%s, %d) " name i
    | VarBound(name, i, j) -> Printf.sprintf "VarBound(%s, %d, %d) " name i j

  and print_report = function
    | [] -> ""
    | h::tl -> Printf.sprintf "%s%s" (print_var h) (print_report tl)

  and print_exp_report = function
    | (reads, writes) -> (Printf.sprintf "\nReads: %s\nWrites: %s\n\n"
                            (print_report reads)
                            (print_report writes))

  and print_exps_report = function
    | [] -> Printf.sprintf ""
    | report::tl -> Printf.sprintf "%s%s" (print_exp_report report) (print_exps_report tl)

  (* format ([], []) *)
  and report_variables_usage vars_to_report e = match e with
    | Const'(cons) -> ([], [])
    | Var'(var) -> report_variables_usage_var vars_to_report var
    | Box'(var) -> ([], [])
    | BoxGet'(var) -> ([], [])
    | BoxSet'(var, rhs) -> ([], [])
    | If'(test, dit, dif) -> report_variables_usage_if vars_to_report test dit dif
    | Seq'(exprs) -> report_variables_usage_expr_list vars_to_report exprs
    | Set'(var, rhs) -> report_variables_usage_set vars_to_report var rhs
    (* TODO: is it ok? it is based on the assumption define cannot be called from inner scope *)
    | Def'(var, rhs) -> ([],[])
    | Or'(exprs) -> report_variables_usage_expr_list vars_to_report exprs
    | LambdaSimple'(params, body) -> report_variables_usage_lambda_simple vars_to_report params body
    | LambdaOpt'(params, opt, body) -> report_variables_usage_lambda_opt vars_to_report params opt body
    | Applic'(func, args) -> report_variables_usage_applic vars_to_report func args
    | ApplicTP'(func, args) -> report_variables_usage_applic vars_to_report func args

  and report_variables_usage_var vars_to_report var = match var with
    | VarParam(var_name, index) -> (
        if (is_var_name_in_varlist var_name vars_to_report)
        then (([VarParam(var_name, index)], []))
        else (([], []))
      )
    | VarBound(var_name, maj, min) -> (
        if (is_var_name_in_varlist var_name vars_to_report)
        then (([VarBound(var_name, maj, min)], []))
        else (([], []))
      )
    | VarFree(var_name) -> ([], [])

  and report_variables_usage_if vars_to_report test dit dif = 
    let test_reports = report_variables_usage vars_to_report test in
    let dit_reports = report_variables_usage vars_to_report dit in
    let dif_reports = report_variables_usage vars_to_report dif in
    combine_several_reports [test_reports; dit_reports; dif_reports]

  and report_variables_usage_expr_list vars_to_report exprs = 
    let exprs_reports = List.map (report_variables_usage vars_to_report) exprs in
    combine_several_reports exprs_reports

  and report_variables_usage_set vars_to_report var rhs = 
    let rhs_report = report_variables_usage vars_to_report rhs in
    let lhs_report = report_variables_usage_var vars_to_report var in
    let fix_lhs_report = (function
        | ([],[]) -> ([],[])
        | ([var], []) -> ([], [var])
        | _ -> raise X_this_should_not_happen) in
    let lhs_report = fix_lhs_report lhs_report in
    combine_two_reports lhs_report rhs_report

  and report_variables_usage_lambda_simple vars_to_report params body = 
    let new_vars_to_report = vars_to_report@params in
    let new_vars_to_report = remove_duplicates new_vars_to_report in
    let body_reports = report_variables_usage new_vars_to_report body in
    (* TODO: i think we want to exclude all the params, right?
              we don't want to report about our new params *)
    (* let exclude_var acc p = if (List.mem p vars_to_report) then (p::acc) else (acc) in *)
    (* let vars_to_exclude = List.fold_left exclude_var [] params in  *)
    let vars_to_exclude = params in
    let rec filter_body_reports = (function
        | ([],[]) -> ([], [])
        | (VarParam(var_name, i)::tl, writes) -> (
            if (List.mem var_name vars_to_exclude) 
            then ( filter_body_reports (tl, writes) )
            else (combine_two_reports ([VarParam(var_name, i)], writes) (filter_body_reports (tl, writes)) )
          )
        | (VarBound(var_name, i, j)::tl, writes) -> (
            if (List.mem var_name vars_to_exclude) 
            then ( filter_body_reports (tl, writes) )
            else (combine_two_reports ([VarBound(var_name, i, j)], writes) (filter_body_reports (tl, writes)) )
          )
        | ([], VarParam(var_name, i)::tl) -> (
            if (List.mem var_name vars_to_exclude) 
            then ( filter_body_reports ([], tl) )
            else (combine_two_reports ([], [VarParam(var_name, i)]) (filter_body_reports ([], tl)) )
          )
        | ([], VarBound(var_name, i, j)::tl) -> (
            if (List.mem var_name vars_to_exclude) 
            then ( filter_body_reports ([], tl) )
            else (combine_two_reports ([], [VarBound(var_name, i, j)]) (filter_body_reports ([], tl)) )
          )
        | _ -> raise X_this_should_not_happen) in
    let body_reports = filter_body_reports body_reports in
    (* Printf.printf "%s" (print_exps_report [body_reports]); body_reports *)
    body_reports

  and report_variables_usage_lambda_opt vars_to_report params opt body =
    let params = params@[opt] in
    report_variables_usage_lambda_simple vars_to_report params body

  and report_variables_usage_applic vars_to_report func args = 
    let args_reports = report_variables_usage_expr_list vars_to_report args in
    let func_report = report_variables_usage vars_to_report func in 
    combine_two_reports func_report args_reports

  (* TODO: Rewrite? *)
  and remove_duplicates lst =
    let rec loop lbuf rbuf =
      match rbuf with
      | [] -> lbuf
      | h::tl ->
        begin
          if List.mem h lbuf then loop lbuf tl
          else loop (h::lbuf) rbuf
        end
    in
    List.rev (loop [] lst)

  and combine_several_reports rep_list = 
    List.fold_left combine_two_reports ([],[]) rep_list

  and combine_two_reports rep1 rep2 = match rep1, rep2 with
    | (reads1, writes1), (reads2, writes2) -> (reads1@reads2, writes1@writes2)

  and is_var_name_in_varlist var_name varlist = List.mem var_name varlist;;

  let box_set e = annotate_box_set e;;

  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)

open Reader;;
open Tag_Parser;;
open Semantics;;

(* Manual Tests

   annotate_lexical_addresses 
   (List.hd (tag_parse_expressions 
    (read_sexprs "(lambda (x y) x y z (lambda (x) x y))")));;

   =>

   LambdaSimple' (["x"; "y"],
   Seq'
   [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1)); Var' (VarFree "z");
   LambdaSimple' (["x"],
    Seq' [Var' (VarParam ("x", 0)); Var' (VarBound ("y", 0, 1))])])


   annotate_lexical_addresses 
   (List.hd (tag_parse_expressions 
   (read_sexprs "(lambda (x y) x y z (lambda (x) (if x y z)))")));;

   =>

   LambdaSimple' (["x"; "y"],
   Seq'
   [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1)); Var' (VarFree "z");
   LambdaSimple' (["x"],
    If' (Var' (VarParam ("x", 0)), Var' (VarBound ("y", 0, 1)),
     Var' (VarFree "z")))])
*)