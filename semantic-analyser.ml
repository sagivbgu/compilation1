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

let untag expr = 
  let rec untag_rec expr is_nested = 
    match expr with
    | Const'(Sexpr(s)) -> unread s
    | Const'(Void) when is_nested -> "#<void>"
    | Const'(Void) -> ""
    | Var'(var) -> untag_var var
    | Box'(var) -> Printf.sprintf "Box(%s)" (untag_var var)
    | BoxGet'(var) -> Printf.sprintf "BoxGet(%s)" (untag_var var)
    | BoxSet'(var, rhs) -> Printf.sprintf "BoxSet(%s, %s)" (untag_var var) (untag_nested rhs)
    | If'(test, dit, dif) -> Printf.sprintf "(if %s %s %s)" (untag_nested test) (untag_nested dit) (untag_nested dif)
    | Seq'(exprs) -> Printf.sprintf "(begin %s)" (untag_list exprs)
    | Or'(exprs) ->  Printf.sprintf "(or %s)" (untag_list exprs)
    | Set'(var, expr2) -> Printf.sprintf "(set! %s %s)" (untag_var var) (untag_nested expr2)
    | Def'(var, expr2) -> Printf.sprintf "(define %s %s)" (untag_var var) (untag_nested expr2)
    | LambdaSimple'(args, expr) -> Printf.sprintf "(lambda (%s) %s)" (String.concat " " args) (untag_nested expr)
    | LambdaOpt'([], arg, expr) -> Printf.sprintf "(lambda %s %s)" arg (untag_nested expr)
    | LambdaOpt'(args, arg, expr) -> Printf.sprintf "(lambda (%s . %s) %s)" (String.concat " " args) arg (untag_nested expr)
    | Applic'(expr, args) -> Printf.sprintf "(%s %s)" (untag_nested expr) (untag_list args) 
    | ApplicTP'(expr, args) -> Printf.sprintf "(TP: %s %s)" (untag_nested expr) (untag_list args) 
  and untag_nested expr = untag_rec expr true 
  and untag_var = function
    | VarFree(name) -> Printf.sprintf "VarFree(%s)" name
    | VarParam(name, i) -> Printf.sprintf "VarParam(%s, %d)" name i
    | VarBound(name, i, j) -> Printf.sprintf "VarBound(%s, %d, %d)" name i j
  and untag_list exprs = String.concat " \n" (List.map untag_nested exprs) in
  untag_rec expr false
  

exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

  let remove_duplicates lst =
    let rec inner prev next = match next with
      | [] -> prev
      | hd::tl -> if List.mem hd prev
        then inner prev tl
        else inner (hd::prev) next
    in
    List.rev (inner [] lst)

  let is_var_of_name var_name expr = match expr with 
    | VarParam(v, _) -> v = var_name
    | VarBound(v, _, _) -> v = var_name
    | VarFree(v) -> v = var_name;;

  let is_var_param = function VarParam(_, _) -> true | _ -> false;;
  let is_var_bound = function VarBound(_, _, _) -> true | _ -> false;;

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
    | _ -> Printf.printf "2\n"; raise X_syntax_error

  and annotate_lex_addr_or varlist expr_list =
    let expr'_list = List.map (annotate_lex_addr_expr varlist) expr_list in
    Or'(expr'_list)

  and annotate_lex_addr_def varlist lhs rhs = match lhs with
    | Var(name) -> Def'(VarFree(name) , (annotate_lex_addr_expr varlist rhs))
    | _ -> Printf.printf "3\n"; raise X_syntax_error

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
    | Const'(cons) -> Const'(cons)
    | Var'(var) -> Var'(var)
    | Box'(var) -> Box'(var)
    | BoxGet'(var) -> BoxGet'(var)
    | BoxSet'(var, rhs) -> BoxSet'(var, rhs)
    | If'(test, dit, dif) -> If'((box_set_expr test), (box_set_expr dit), (box_set_expr dif))
    | Seq'(exprs) -> Seq'((List.map box_set_expr exprs))
    | Set'(var, rhs) -> Set'(var, (box_set_expr rhs))
    | Def'(var, rhs) -> Def'(var, (box_set_expr rhs))
    | Or'(exprs) -> Or'((List.map box_set_expr exprs))
    | LambdaSimple'(params, body) -> LambdaSimple'(params, (box_set_lambda_body params body))
    | LambdaOpt'(params, opt, body) -> LambdaOpt'(params, opt, (box_set_lambda_opt_body params opt body))
    | Applic'(func, args) -> Applic'((box_set_expr func), (List.map box_set_expr args))
    | ApplicTP'(func, args) -> ApplicTP'((box_set_expr func), (List.map box_set_expr args))

  and flatten_applics_and_ors = function
    | Seq'(exprs) -> Seq'(List.flatten (List.map extract_applic_and_or exprs))
    | x -> Seq'((extract_applic_and_or x))

  and extract_applic_and_or = function
    | Applic'(func, args) -> func::args
    | ApplicTP'(func, args) -> func::args
    | Or'(exprs) -> exprs
    | x -> [x]

  and box_set_lambda_opt_body params opt body =
    let params = params@[opt] in 
    box_set_lambda_body params body 

  and box_set_lambda_body params body = 
    let params_to_report = params in 
    let flattened_body = flatten_applics_and_ors body in
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

      let rec same_var_bound_in_different_read_and_write prev_reads prev_writes reads writes =
        let check_read reads all_writes =
          let writes = List.flatten all_writes in
          let same_var_bounds = List.filter (fun var_bound -> List.mem var_bound writes) reads in
          List.length same_var_bounds > 0 in
        match reads, writes with
        | [], [] -> false
        | [], (w :: rest_w) -> false
        | (r :: rest_r), [] -> false
        | (r :: rest_r), (w :: rest_w) -> (check_read r (prev_writes @ rest_w)) || (same_var_bound_in_different_read_and_write (prev_reads @ [r]) (prev_writes @ [w]) rest_r rest_w)
      in

      if (reads_var_params_amount > 0 && writes_var_bounds_amount > 0) ||
         (writes_var_params_amount > 0 && reads_var_bounds_amount > 0)
      then not (is_expr_special_boxing_criteria body param) 
      else if (reads_var_params_amount = 0 || writes_var_params_amount = 0) &&
              reads_var_bounds_amount > 0 &&
              writes_var_bounds_amount > 0 &&
              (same_var_bound_in_different_read_and_write [] [] reads_var_bounds writes_var_bounds)
      then true
      else false in
      (* TODO _ REMOVEEEEEEEEEEEEE THIS IS FOR DEBUGGING - WE BOX ALL VARS *)
    (* let vars_to_box = List.filter (check_need_boxing subexps_reports) params_to_report in *)
    let vars_to_box = List.filter ((fun a b -> true) subexps_reports) params_to_report in

    let new_body = box_var_in_lambda_body vars_to_box params body in
    let new_body = box_set_expr new_body in
    new_body

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
      let reads = List.filter is_var_bound reads in
      List.length reads > 0) in
    let is_expr_deep_write_occur variable_usage_report = (
      let (reads, writes) = variable_usage_report in
      let writes = List.filter is_var_bound writes in
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
        (* let report = if write_occurred then (report_variable_usage (set_rhs expr)) else report in *)
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
    (* Based on the assumption that define cannot be called from an inner scope *)
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
    body_reports

  and report_variables_usage_lambda_opt vars_to_report params opt body =
    let params = params@[opt] in
    report_variables_usage_lambda_simple vars_to_report params body

  and report_variables_usage_applic vars_to_report func args = 
    let args_reports = report_variables_usage_expr_list vars_to_report args in
    let func_report = report_variables_usage vars_to_report func in 
    combine_two_reports func_report args_reports

  and combine_several_reports rep_list = 
    List.fold_left combine_two_reports ([],[]) rep_list

  and combine_two_reports rep1 rep2 = match rep1, rep2 with
    | (reads1, writes1), (reads2, writes2) -> (reads1@reads2, writes1@writes2)

  and is_var_name_in_varlist var_name varlist = List.mem var_name varlist

  and box_var_in_lambda_body vars_to_box params body = 
    let boxed_body = List.fold_left box_var_in_expr body vars_to_box in
    let set_expr var_name = 
      let index = get_param_index var_name params in
      (Set'(VarParam(var_name, index), Box'(VarParam(var_name, index)))) in
    let set_exprs = List.map set_expr vars_to_box in
    let new_body set_exprs boxed_body = (match set_exprs, boxed_body with
        | _, Seq'(exprs) -> let exprs = set_exprs@exprs in Seq'(exprs)
        | [], expr -> expr
        | _, expr -> let exprs = set_exprs@[expr] in Seq'(exprs)) in
    new_body set_exprs boxed_body

  and box_var_in_expr expr var_name = match expr with
    | Const'(cons) -> Const'(cons)
    | Var'(var) -> box_var_in_var var var_name
    | Box'(var) -> Box'(var)
    | BoxGet'(var) -> BoxGet'(var)
    | BoxSet'(var, rhs) -> BoxSet'(var, (box_var_in_expr rhs var_name))
    | If'(test, dit, dif) -> box_var_in_if var_name test dit dif
    | Seq'(exprs) -> box_var_in_seq var_name exprs
    | Set'(var, rhs) -> box_var_in_set var_name var (box_var_in_expr rhs var_name)
    | Def'(var, rhs) -> Def'(var, rhs)
    | Or'(exprs) -> box_var_in_or var_name exprs
    | LambdaSimple'(params, body) -> box_var_in_lambda_simple var_name params body
    | LambdaOpt'(params, opt, body) -> box_var_in_lambda_opt var_name params opt body
    | Applic'(func, args) -> box_var_in_applic var_name func args
    | ApplicTP'(func, args) -> box_var_in_applictp var_name func args

  and box_var_in_var var var_name = match var with
    | VarFree(_) -> Var'(var)
    | VarParam(name, _) -> if (String.equal name var_name) then (BoxGet'(var)) else (Var'(var))
    | VarBound(name, _, _) -> if (String.equal name var_name) then (BoxGet'(var)) else (Var'(var))

  and box_var_in_if var_name test dit dif = 
    If'((box_var_in_expr test var_name), 
        (box_var_in_expr dit var_name),
        (box_var_in_expr dif var_name))

  and box_var_in_seq var_name exprs = 
    let box_var_in_expr exp = box_var_in_expr exp var_name in
    let exprs = List.map box_var_in_expr exprs in
    Seq'(exprs)

  and box_var_in_set var_name var rhs = match var with
    | VarFree(_) -> Set'(var, rhs)
    | VarParam(name, _) -> if (String.equal name var_name) then (BoxSet'(var, rhs)) else (Set'(var, rhs))
    | VarBound(name, _, _) -> if (String.equal name var_name) then (BoxSet'(var, rhs)) else (Set'(var, rhs))

  and box_var_in_or var_name exprs =
    let box_var_in_expr exp = box_var_in_expr exp var_name in
    let exprs = List.map box_var_in_expr exprs in
    Or'(exprs)

  and box_var_in_lambda_simple var_name params body = 
    (* If the var_name is in params, we don't want to box it*)
    if (List.mem var_name params)
    then (LambdaSimple'(params, body))
    else (LambdaSimple'(params, (box_var_in_expr body var_name)))

  and box_var_in_lambda_opt var_name params opt body =
    let new_params = params@[opt] in 
    if (List.mem var_name new_params)
    then (LambdaOpt'(params, opt, body))
    else (LambdaOpt'(params, opt, (box_var_in_expr body var_name)))

  and box_var_in_applic var_name func args = 
    let box_var_in_expr exp = box_var_in_expr exp var_name in
    let args = List.map box_var_in_expr args in
    Applic'((box_var_in_expr func), args)

  and box_var_in_applictp var_name func args = 
    let box_var_in_expr exp = box_var_in_expr exp var_name in
    let args = List.map box_var_in_expr args in
    ApplicTP'((box_var_in_expr func), args)

  let box_set e = annotate_box_set e;;

  let run_semantics expr =
    box_set
      (annotate_tail_calls 
         (annotate_lexical_addresses expr)
         )
         ;;

end;; (* struct Semantics *)
