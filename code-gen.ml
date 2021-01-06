#use "semantic-analyser.ml";;
#load "str.cma";;

let untag e = Str.global_replace (Str.regexp "\n") " " (untag e);;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string

end;;

module Code_Gen : CODE_GEN = struct
  let get_operation_index =
      let index = ref 0 in
      fun () ->
        incr index;
        !index;;

  let make_consts_tbl asts = 
    (* This is the begining of the consts table, no matter what the program is *)
    let init_consts_tbl = [Void; Sexpr(Nil); Sexpr(Bool false); Sexpr(Bool true)] in
    
    (* expr' -> const list *)
    (* This method takes an expr' and returns a constant list of its sub-expr's *)
    let rec expr_to_const_list expr = match expr with
      | Const'(const) -> [const]
      | BoxSet'(_, rhs) -> expr_to_const_list rhs
      | If'(test, dit, dif) -> if_to_const_list test dit dif
      | Seq'(exprs) -> List.flatten (List.map expr_to_const_list exprs) 
      | Set'(_, rhs) -> expr_to_const_list rhs
      | Def'(_, rhs) -> expr_to_const_list rhs
      | Or'(exprs) -> List.flatten (List.map expr_to_const_list exprs) 
      | LambdaSimple'(_, body) -> expr_to_const_list body
      | LambdaOpt'(_, _, body) -> expr_to_const_list body
      | Applic'(func, args) -> applic_to_const_list func args
      | ApplicTP'(func, args) -> applic_to_const_list func args
      | _ -> []
    
      and if_to_const_list test dit dif = 
        let test_consts = expr_to_const_list test in
        let dit_consts = expr_to_const_list dit in
        let dif_consts = expr_to_const_list dif in
        test_consts @ dit_consts @ dif_consts
      
      and applic_to_const_list func args = 
        let func_consts = expr_to_const_list func in
        let args_consts = List.flatten (List.map expr_to_const_list args) in
        func_consts @ args_consts in 
    
    (* this method is const -> const list *)
    (* it extends the subconstants of complex sexprs *)
    let rec extend_sub_constant = function
      | Void -> [Void]
      (* If that constant is a symbol, add the String of that symbol to the table, prior to the
          symbol. *)
      | Sexpr(Symbol(sym)) -> [Sexpr(String(sym)); Sexpr(Symbol(sym))]
      | Sexpr(Pair(car, cdr)) -> extend_pair_constants car cdr
      | sexpr -> [sexpr]
      (* If it’s a pair, add the car and cdr before the pair itself. Apply the extension procedure
          recursively to the car and cdr *)
      and extend_pair_constants car cdr = 
        let extended_car = extend_sub_constant (Sexpr(car)) in
        let extended_cdr = extend_sub_constant (Sexpr(cdr)) in
        extended_car @ extended_cdr @ [Sexpr(Pair(car, cdr))] in
    
    (* const list -> const list | removes duplicates, leaves only first instance *)
    let remove_duplicates const_lst = 
      let rec is_const_in_new_lst const lst = match const, lst with 
        | _, [] -> false
        | Void, Void::tl -> true
        | Void, _::tl -> is_const_in_new_lst const tl
        | Sexpr(exp1), Sexpr(exp2)::tl -> if (sexpr_eq exp1 exp2) then true else (is_const_in_new_lst const tl)
        | Sexpr(_), Void::tl -> is_const_in_new_lst const tl
        in
      List.fold_left (fun new_lst const -> 
                            if (is_const_in_new_lst const new_lst)
                            then new_lst
                            else new_lst@[const]) [] const_lst in
      
    (* const -> int | Calculate the size of const *)
    let const_size const = match const with
      | Void -> 1
      | Sexpr(Nil) -> 1
      | Sexpr(Bool(_)) -> 1 + 1
      | Sexpr(Char(_)) -> 1 + 1
      | Sexpr(String(str)) -> 1 + 8 + String.length str
      | Sexpr(Symbol(_)) -> 1 + 8
      | Sexpr(Pair(_,_)) -> 1 + 8 + 8
      | Sexpr(Number(Float(_))) -> 1 + 8
      | Sexpr(Number(Fraction(_,_))) -> 1 + 8 + 8 in

    (* int * const list -> (const * int) list | calculate the offset of the const*)
    let rec calc_offsets_of_consts offset consts_lst = match consts_lst with
      | [] -> []
      | const::rest -> [const, offset] @ (calc_offsets_of_consts ((const_size const) + offset) rest) in
    (* const list -> (const * int) list | calculate offset for all the consts *)
    let to_consts_and_offsets consts = calc_offsets_of_consts 0 consts in
    (* const -> (const * int) list -> int | find offset of const in list *)
    let get_offset_of_const const const_lst = List.assoc const const_lst in
    
    (* sub-methods for make_const_row  *)
    let make_row_cmd_with_comment cmd offset const_str = 
      Printf.sprintf "%s\t; offset %d, %s" cmd offset const_str in

    let make_char_row offset ch = 
      let cmd = Printf.sprintf "MAKE_LITERAL_CHAR(%d)" (int_of_char ch) in
      let comment = 
        if ((int_of_char ch) <= 32) 
        then (Printf.sprintf "char %d" (int_of_char ch))
        else (Printf.sprintf "'%c'" ch) in
      let cmd = make_row_cmd_with_comment cmd offset comment in
      (Sexpr(Char(ch)), (offset, cmd)) in
    
    let make_str_row offset str = 
      let cmd = Printf.sprintf "MAKE_LITERAL_STRING \"%s\"" str in
      let cmd = make_row_cmd_with_comment cmd offset (Printf.sprintf "\"%s\"" str) in
      (Sexpr(String(str)), (offset, cmd)) in
    
    let make_symbol_row offset sym consts_lst = 
      let sym_index = get_offset_of_const (Sexpr(String(sym))) consts_lst in
      let cmd = Printf.sprintf "MAKE_LITERAL_SYMBOL(const_tbl+%d)" sym_index in
      let cmd = make_row_cmd_with_comment cmd offset sym in
      (Sexpr(Symbol(sym)), (offset, cmd)) in
    
    let make_pair_row offset car cdr consts_lst = 
      let car_index = get_offset_of_const (Sexpr(car)) consts_lst in
      let cdr_index = get_offset_of_const (Sexpr(cdr)) consts_lst in
      let cmd = Printf.sprintf "MAKE_LITERAL_PAIR(const_tbl+%d, const_tbl+%d)" car_index cdr_index in
      let cmd = make_row_cmd_with_comment cmd offset (Printf.sprintf "(%s, %s)" (unread car) (unread cdr)) in
      (Sexpr(Pair(car,cdr)), (offset, cmd)) in
    
    let make_float_row offset fl = 
      let cmd = Printf.sprintf "MAKE_LITERAL_FLOAT(%f)" fl in
      let cmd = make_row_cmd_with_comment cmd offset (Printf.sprintf "%f" fl) in
      (Sexpr(Number(Float(fl))), (offset, cmd)) in
    
    let make_rational_row offset num den = 
      let cmd = Printf.sprintf "MAKE_LITERAL_RATIONAL(%d, %d)" num den in
      let cmd = make_row_cmd_with_comment cmd offset (Printf.sprintf "%d/%d" num den) in
      (Sexpr(Number(Fraction(num,den))), (offset, cmd)) in
    
    (* (const * int) -> (const * (int * string)) | create a row in the table*)
    let make_const_row consts_lst const_and_offset = match const_and_offset with
      | (Void, offset) -> (Void, (offset, (make_row_cmd_with_comment "db T_VOID" offset "#<void>")))
      | (Sexpr(Nil), offset) -> (Sexpr(Nil), (offset, (make_row_cmd_with_comment "db T_NIL" offset "()")))
      | (Sexpr(Bool(false)), offset) -> (Sexpr(Bool(false)), (offset, (make_row_cmd_with_comment "db T_BOOL, 0" offset "#f")))
      | (Sexpr(Bool(true)), offset) -> (Sexpr(Bool(true)), (offset, (make_row_cmd_with_comment "db T_BOOL, 1" offset "#t")))
      | (Sexpr(Char(ch)), offset) -> make_char_row offset ch
      | (Sexpr(String(str)), offset) -> make_str_row offset str
      | (Sexpr(Symbol(sym)), offset) -> make_symbol_row offset sym consts_lst
      | (Sexpr(Pair(car,cdr)), offset) -> make_pair_row offset car cdr consts_lst
      | (Sexpr(Number(Float(fl))), offset) -> make_float_row offset fl 
      | (Sexpr(Number(Fraction(num,den))), offset) -> make_rational_row offset num den in
    
    (* expr' list -> const list - each const is extended*)
    let exprs_to_extended_const_list exprs =
      let consts = List.flatten (List.map expr_to_const_list exprs) in
      let consts = (List.fold_right 
        (fun const const_lst -> (extend_sub_constant const) @ const_lst)
        consts
        []) in
      init_consts_tbl @ consts in

    (* The main logic - combine all together *)
    (* const list *)
    let consts_tbl = exprs_to_extended_const_list asts in
    (* const list - without duplicates *)
    let consts_tbl = remove_duplicates consts_tbl in
    (* (const * int) list *)
    let consts_tbl = to_consts_and_offsets consts_tbl in
    (* (const * (int * string)) list *)
    let consts_tbl = List.map (make_const_row consts_tbl) consts_tbl in
    consts_tbl;;

  (* expr' list -> (string * int) list *)
  let make_fvars_tbl asts = 
    (* expr' -> string list *)
    (* This method takes an expr' and returns a list of free variable names in its sub-expr's *)
    let rec expr_to_fvar_names expr = match expr with
      | Var'(VarFree(v)) -> [v]
      | BoxSet'(_, rhs) -> expr_to_fvar_names rhs
      | If'(test, dit, dif) -> if_to_fvar_names test dit dif
      | Seq'(exprs) -> List.flatten (List.map expr_to_fvar_names exprs) 
      | Set'(VarFree(fvar), rhs) -> fvar :: expr_to_fvar_names rhs
      | Set'(_, rhs) -> expr_to_fvar_names rhs
      | Def'(VarFree(fvar), rhs) -> fvar :: expr_to_fvar_names rhs
      | Def'(_, rhs) -> expr_to_fvar_names rhs
      | Or'(exprs) -> List.flatten (List.map expr_to_fvar_names exprs) 
      | LambdaSimple'(_, body) -> expr_to_fvar_names body
      | LambdaOpt'(_, _, body) -> expr_to_fvar_names body
      | Applic'(func, args) -> applic_to_fvar_names func args
      | ApplicTP'(func, args) -> applic_to_fvar_names func args
      | _ -> []
    
      and if_to_fvar_names test dit dif = 
        let test_consts = expr_to_fvar_names test in
        let dit_consts = expr_to_fvar_names dit in
        let dif_consts = expr_to_fvar_names dif in
        test_consts @ dit_consts @ dif_consts
      
      and applic_to_fvar_names func args = 
        let func_fvars = expr_to_fvar_names func in
        let args_fvars = List.flatten (List.map expr_to_fvar_names args) in
        func_fvars @ args_fvars in 
    
    (* string list -> string list | removes duplicates, leaves only first instance *)
    let remove_duplicates fvars = 
      let rec is_fvar_in_new_lst fvar lst = match fvar, lst with 
        | _, [] -> false
        | fvar, hd::tl -> if String.equal fvar hd then true else is_fvar_in_new_lst fvar tl
      in
      List.fold_left (fun new_lst fvar -> 
                            if (is_fvar_in_new_lst fvar new_lst)
                            then new_lst
                            else new_lst@[fvar]) [] fvars in

    (* string list -> (string * int) list | calculate offset for all the free vatiables *)
    let to_fvars_and_offsets fvars =
      let rec list_to_item_and_index_tuples lst idx results = match lst with
        | [] -> results
        | hd :: tl -> list_to_item_and_index_tuples tl (idx + 1) (results @ [(hd, idx)]) in
    list_to_item_and_index_tuples fvars 0 [] in
    
    (* The main logic - combine all together *)
    (* string list *)
    let fvars = List.flatten (List.map expr_to_fvar_names asts) in
    (* string list - without duplicates *)
    let fvars = remove_duplicates fvars in
    (* (string * int) list *)
    let fvars = to_fvars_and_offsets fvars in
    fvars;;
  
  let generate consts fvars e =
    let get_commented_cmd_string operation_description cmd =
      let pre_comment = Printf.sprintf ";; Starting: %s" operation_description in
      let post_comment = Printf.sprintf ";; Finished: %s\n" operation_description in
      Printf.sprintf "%s\n%s\n%s" pre_comment cmd post_comment in

    let generate_const consts_tbl const = 
      let index_and_cmd = List.assoc const consts_tbl in
      let extract_index (i, c) = i in
      let index = extract_index index_and_cmd in
      let cmd = Printf.sprintf "mov rax, const_tbl+%d" index in
      let cmd_with_comment = Printf.sprintf "%s\t; mov const %s to rax" cmd (untag (Const'(const))) in
      cmd_with_comment in

    let generate_fvar_get fvars_tbl fvar =
      let index = List.assoc fvar fvars_tbl in
      let cmd = Printf.sprintf "mov rax, qword [fvar_tbl+8*%d]" index in
      let cmd_with_comment = Printf.sprintf "%s\t; mov fvar %s to rax" cmd fvar in
      cmd_with_comment in
    
    let get_lambda_depth =
      let depth = ref (-1) in
      fun () ->
        incr depth;
        !depth in

    let rec generate_exp consts fvars expr = match expr with
      | Const'(const) -> generate_const consts const
      | Var'(VarFree(fvar)) -> generate_fvar_get fvars fvar
      | Set'(VarFree(fvar), e) -> generate_fvar_set consts fvars fvar e
      | Def'(VarFree(fvar), e) -> generate_fvar_set consts fvars fvar e
      | If'(test, dit, dif) -> generate_if consts fvars test dit dif
      | Or'(exprs) -> generate_or consts fvars exprs
      | Seq'(exprs) -> generate_seq consts fvars exprs
      | Var'(VarParam(param_name, minor)) -> generate_get_param param_name minor
      | Set'(VarParam(param_name, minor), rhs) -> generate_set_param consts fvars param_name minor rhs
      | Var'(VarBound(var_name, major, minor)) -> generate_get_var_bound var_name major minor
      | Set'(VarBound(var_name, major, minor), rhs) -> generate_set_var_bound consts fvars var_name major minor rhs
      | BoxGet'(v) -> generate_box_get consts fvars v
      | BoxSet'(v, rhs) -> generate_box_set consts fvars v rhs
      | LambdaSimple'(p_names, body) -> generate_lambda_simple consts fvars p_names body
      | Applic'(proc, args) -> generate_applic consts fvars proc args
      | Box'(v) -> generate_box consts fvars v
      | ApplicTP'(proc, args) -> generate_applic_tp consts fvars proc args
      (* The expression will appear in the .s file *)
      | _ -> Printf.printf "\n\nCheck: %s\n\n" (untag expr); raise X_not_yet_implemented
    
    and generate_fvar_set consts_tbl fvars_tbl fvar expr =
      let operation_description = Printf.sprintf "Set fvar %s to %s" fvar (untag expr) in
      
      let expr_eval_cmd = generate_exp consts_tbl fvars_tbl expr in
      
      let index = List.assoc fvar fvars_tbl in
      let cmd_end = Printf.sprintf "mov qword [fvar_tbl+8*%d], rax \nmov rax, SOB_VOID_ADDRESS" index in
      
      let cmd = expr_eval_cmd ^ "\n" ^ cmd_end in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
      
    and generate_if consts fvars test dit dif =
      let operation_index = get_operation_index() in
      let operation_description = Printf.sprintf "If statement (#%d): %s" operation_index (untag (If'(test, dit, dif))) in
      
      let if_label = Printf.sprintf "Lif%d" operation_index in
      let else_label = Printf.sprintf "Lelse%d" operation_index in
      let end_if_label = Printf.sprintf "LendIf%d" operation_index in

      let test_eval_cmd = generate_exp consts fvars test in
      let dit_eval_cmd = generate_exp consts fvars dit in
      let dif_eval_cmd = generate_exp consts fvars dif in
            
      let cmd =
if_label ^ ":
" ^ test_eval_cmd ^ "
cmp rax, SOB_FALSE_ADDRESS
je " ^ else_label ^ "
" ^ dit_eval_cmd ^ "
jmp " ^ end_if_label ^ "
" ^ else_label ^ ":
" ^ dif_eval_cmd ^ "
" ^ end_if_label ^ ":" in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
      
    (*   Lor:
        [[E1]]
        cmp rax, SOB_FALSE_ADDRESS
        jne LendOr
        [[E2]]
        cmp rax, SOB_FALSE_ADDRESS
        jne LendOr
        ...
        [[En]]
        LendOr:              *)
    and generate_or consts fvars exprs =
      let operation_index = get_operation_index() in
      let operation_description = Printf.sprintf "Or statement (#%d): %s" operation_index (untag (Or'(exprs))) in
      
      let or_label = Printf.sprintf "Lor%d" operation_index in
      let end_or_label = Printf.sprintf "LendOr%d" operation_index in

      let get_or_item_description index = Printf.sprintf "Item %d in Or statement #%d" index operation_index in
      let wrap_expr_cmd expr_index expr_cmd =
        (* Wrap an expr evaluation with useful debug information *)
        get_commented_cmd_string (get_or_item_description expr_index) expr_cmd in
      let exprs_eval_cmds = List.map (generate_exp consts fvars) exprs in
      let exprs_eval_cmds = List.mapi wrap_expr_cmd exprs_eval_cmds in

      let check_false_cmd = Printf.sprintf "cmp rax, SOB_FALSE_ADDRESS \njne %s" end_or_label in      
      let cmd = String.concat check_false_cmd exprs_eval_cmds in
      let cmd = Printf.sprintf "%s: \n%s \n%s:" or_label cmd end_or_label in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_seq consts fvars exprs =
      let operation_index = get_operation_index() in
      let operation_description = Printf.sprintf "Seq statement #%d: %s" operation_index (untag (Seq'(exprs))) in
      let get_seq_item_description index = Printf.sprintf "Item %d in Seq statement #%d" index operation_index in
      let wrap_expr_cmd expr_index expr_cmd =
        (* Wrap an expr evaluation with useful debug information *)
        get_commented_cmd_string (get_seq_item_description expr_index) expr_cmd in
      let exprs_eval_cmds = List.map (generate_exp consts fvars) exprs in
      let exprs_eval_cmds = List.mapi wrap_expr_cmd exprs_eval_cmds in
      let cmd = String.concat "\n\n" exprs_eval_cmds in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_get_param param_name minor = 
      let operation_description = Printf.sprintf "Get VarParam(name=%s, minor=%d)" param_name minor in
      let cmd = Printf.sprintf "mov rax, PVAR(%d)" minor in 
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_set_param consts fvars param_name minor rhs = 
      let operation_description = Printf.sprintf "Set VarParam(name=%s, minor=%d) with %s" param_name minor (untag rhs) in 
      let rhs_operation_description = Printf.sprintf "Evaluating rhs for [ %s ], value is expected in rax" operation_description in
      let rhs_cmd = generate_exp consts fvars rhs in
      let rhs_cmd_with_comment = get_commented_cmd_string rhs_operation_description rhs_cmd in
      let assign_cmd = Printf.sprintf "mov PVAR(%d), rax\nmov rax, SOB_VOID_ADDRESS" minor in
      let cmd = rhs_cmd_with_comment ^ "\n" ^ assign_cmd in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_get_var_bound var_name major minor = 
      let operation_description = Printf.sprintf "Get VarBound(name=%s, major=%d, minor=%d)" var_name major minor in
      let cmd_1 = Printf.sprintf "mov rax, qword [rbp + 8 * 2]" in
      let cmd_2 = Printf.sprintf "mov rax, qword [rax + 8 * %d]" major in
      let cmd_3 = Printf.sprintf "mov rax, qword [rax + 8 * %d]" minor in
      let cmd = cmd_1 ^ "\n" ^ cmd_2 ^ "\n" ^ cmd_3 in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_set_var_bound consts fvars var_name major minor rhs =
      let operation_description = Printf.sprintf "Set VarBound(name=%s, major=%d, minor=%d) with %s" var_name major minor (untag rhs) in 
      let rhs_operation_description = Printf.sprintf "Evaluating rhs [ %s ], value is expected in rax" operation_description in
      let rhs_cmd = generate_exp consts fvars rhs in
      let rhs_cmd_with_comment = get_commented_cmd_string rhs_operation_description rhs_cmd in
      let assign_cmd_1 = Printf.sprintf "mov rbx, qword [rbp + 8 ∗ 2]" in
      let assign_cmd_2 = Printf.sprintf "mov rbx, qword [rbx + 8 ∗ %d]" major in
      let assign_cmd_3 = Printf.sprintf "mov qword [rbx + 8 ∗ %d], rax" minor in
      let assign_cmd_4 = Printf.sprintf "mov rax, SOB_VOID_ADDRESS" in
      let assign_cmd = "\n" ^ assign_cmd_1 ^ "\n" ^ assign_cmd_2 ^ "\n" ^ assign_cmd_3 ^ "\n" ^ assign_cmd_4 in
      let cmd = rhs_cmd_with_comment ^ assign_cmd in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_box consts fvars v = 
      let operation_description = Printf.sprintf "Creating Box for Var: %s" (untag (Var'(v))) in
      let get_var_code = generate_exp consts fvars (Var'(v)) in
      let save_var_value = "; Save the variable value in rbx\npush rbx\nmov rbx, rax" in
      let allocate_qword_for_var = "; Allocate one qword for the pointer\nMALLOC rax, 8" in
      let put_var_in_address = "; Put var in the address\nmov qword [rax], rbx" in
      let reverse_rbx = "; Reverse rbx\npop rbx" in
      let final_comment = "; address of box in RAX as expected" in
      let code = 
        String.concat "\n" 
          [get_var_code; save_var_value; allocate_qword_for_var; put_var_in_address;
          reverse_rbx; final_comment] in
      let code_with_comment = get_commented_cmd_string operation_description code in
      code_with_comment

    and generate_box_get consts fvars v = 
      let operation_description = Printf.sprintf "BoxGet of %s" (untag (Var'(v))) in
      let var_desc = Printf.sprintf "Evaluating val for [ %s ], the VALUE is expected in rax" operation_description in
      let var_cmd = generate_exp consts fvars (Var'(v)) in
      let var_cmd_with_comment = get_commented_cmd_string var_desc var_cmd in
      let assign_cmd = "; now the box address is in RAX, get the value from inside the box\nmov rax, qword [rax]" in
      let cmd = var_cmd_with_comment ^ "\n" ^ assign_cmd in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_box_set consts fvars v rhs = 
      let operation_description = Printf.sprintf "BoxSet of %s with %s" (untag (Var'(v))) (untag rhs) in
      let rhs_desc = Printf.sprintf "Evaluating rhs for [ %s ], the VALUE is expected in rax" operation_description in
      let rhs_cmd = generate_exp consts fvars rhs in
      let rhs_cmd_with_comment = get_commented_cmd_string rhs_desc rhs_cmd in
      let var_desc = Printf.sprintf "Evaluating var for [ %s ], the VALUE is expected in rax" operation_description in
      let var_cmd = generate_exp consts fvars (Var'(v)) in
      let var_cmd_with_comment = get_commented_cmd_string var_desc var_cmd in
      let cmd_2 = "; Save the value of rhs\npush rax" in
      let cmd_4 = "; in RAX there is a address of a box, put the value we saved on stack earlier (from rhs) inside it\npop qword [rax]" in
      let cmd_5 = "mov rax, SOB_VOID_ADDRESS" in
      let cmd = String.concat "\n" [rhs_cmd_with_comment; cmd_2 ; var_cmd_with_comment ; cmd_4 ; cmd_5] in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_lambda_simple consts fvars p_names body = 
      (* Init *)
      let operation_index = get_operation_index() in  
      let operation_description = Printf.sprintf "Creating CLOSURE of LambdaSimple#%d -> %s" operation_index (untag (LambdaSimple'(p_names, body))) in 
      let depth = get_lambda_depth() in
      let lcont_label =  Printf.sprintf "LClosureCont%d" operation_index in
      let lcode_label =  Printf.sprintf "LClosureCode%d" operation_index in
      
      (* Allocate NewExtEnv *)
      let envextend_label = Printf.sprintf "LClosureEnvExtend%d" operation_index in
      let envextend_end_label = Printf.sprintf "LClosureEnvExtend_End%d" operation_index in
      let envextend_loop_label =  Printf.sprintf "LClosureEnvExtend_Loop%d" operation_index in
      let envextend_loop_end_label =  Printf.sprintf "LClosureEnvExtend_LoopEnd%d" operation_index in
      let envextend_params_loop_label = Printf.sprintf "LClosureEnvExtend_ParamsLoop%d" operation_index in
      let envextend_params_loop_end_label = Printf.sprintf "LClosureEnvExtend_ParamsLoopEnd%d" operation_index in
      let bytes_to_allocate = depth * 8 in
      
      (* Check Depth Code *)
      let check_depth_code = Printf.sprintf 
"; Env Extend Code Section For Closure#%d
mov rbx, %d ; move the depth (== length of env on stack) to rbx
cmp rbx, 0
jne %s
; if it's equal, we are at the first level
; so there is no env to extend
mov rbx, SOB_NIL_ADDRESS
jmp %s
" operation_index depth envextend_label envextend_end_label in

      let envextend_label_code = Printf.sprintf
"%s:
  ; rbx = the depth == the length of the ext_env
  
  ; store registers for personal use
  push rax
  push rcx
  push rdx

  ; store in rcx the length of the env on the stack
  mov rcx, %d
  ; rax = nothing
  ; rbx = the depth == the length of the ext_env
  ; rcx = length of env on stack
  ; rdx = nothing

  ; get the address of the current major list (on stack)
  mov rbx, qword [ENV_STACK_POSITION]
  ; rax = nothing
  ; rbx = address of env major list
  ; rcx = length of env
  ; rdx = nothing

  ; allocate the ext_env major list
  MALLOC rax, %d
  ; rax = address of ext_env major list
  ; rbx = address of env major list
  ; rcx = length of env
  ; rdx = nothing
" envextend_label (depth - 1) bytes_to_allocate in
      let envextend_loop_label_code = Printf.sprintf 
" ; LOOP
  cmp rcx, 0
%s:
  jz %s
  mov rdx, rcx
  ; rax = address of ext_env major list
  ; rbx = address of env major list
  ; rcx = length of env
  ; rdx = length of env

  dec rcx
  ; rax = address of ext_env major list
  ; rbx = address of env major list
  ; rcx = length of env - 1
  ; rdx = length of env

  ; annotate rcx = n, so rdx = n + 1
  ; need to do: rax + WORD_SIZE * (n + 1) <- rbx + WORD_SIZE * n
  ; DOING : mov qword [rax + rdx*WORD_SIZE], qword [rbx + rcx*WORD_SIZE]
  ; store registers for personal use
  push r8
  push r9

  lea r8, [rax + rdx*WORD_SIZE]
  lea r9, [rbx + rcx*WORD_SIZE]
  mov r9, qword [r9]
  mov qword [r8], r9

  ; revert registers used for personal use
  pop r9
  pop r8

  jmp %s
" envextend_loop_label envextend_loop_end_label envextend_loop_label in
      
      let envextend_loop_end_label_code = Printf.sprintf
"%s:
  ; rax = address of ext_env major list
  ; rbx = address of env major list
  ; rcx = 0
  ; rdx = 1

  ; copy value of rax
  mov rbx, rax
  ; rax = address of ext_env major list
  ; rbx = address of ext_env major list
  ; rcx = 0
  ; rdx = 1

  ; get the args_count from stack
  mov rcx, qword [ARGS_COUNT_POSITION]
  ; rax = address of ext_env major list
  ; rbx = address of ext_env major list
  ; rcx = args number
  ; rdx = 1

  ; check if there are params to copy
  cmp rcx, 0
  je %s
  
  ; calculate the number of bytes to allocate for extenv[0]
  mov rax, rcx
  mov rdx, WORD_SIZE
  imul rdx
  ; rax = number of bytes to allocate for extenv[0]
  ; rbx = address of ext_env major list
  ; rcx = args number
  ; rdx = 8

  ; allocate extenv[0] rib
  MALLOC rax, rax
  ; rax = address of extenv[0] rib
  ; rbx = address of ext_env major list
  ; rcx = args number
  ; rdx = 8

  ; link address of extenv[0] into ext_env major list in position 0
  mov qword [rbx], rax
  ; rax = address of extenv[0] rib
  ; rbx = address of ext_env major list (now with all ribs linked)
  ; rcx = args number
  ; rdx = 8
" envextend_loop_end_label envextend_params_loop_end_label in

      let envextend_params_loop_label_code = Printf.sprintf 
"; COPY PARAMS LOOP
cmp rcx, 0
%s:
  jz %s

  dec rcx
  ; rax = address of extenv[0] rib
  ; rbx = address of ext_env major list (now with all ribs linked)
  ; rcx = args number - 1 [first iteration, from second: rcx = rcx - 1]
  ; rdx = 8

  ; annotate rcx = n
  ; need to do rax + WORD_SIZE * n <- PVAR(n)
  push r8
  mov r8, PVAR(rcx)
  mov qword [rax + WORD_SIZE*rcx], r8
  pop r8

  jmp %s
" envextend_params_loop_label envextend_params_loop_end_label envextend_params_loop_label in

      let envextend_params_loop_end_label_code = Printf.sprintf
"%s:
  ; rax = address of extenv[0] rib
  ; rbx = address of ext_env major list (now with all ribs linked)
  ; rcx = 0
  ; rdx = 8

  ; we used these regs at the begining of the extension, so pop them back
  pop rdx
  pop rcx
  pop rax
%s:
  ; address is in RBX as expected
" envextend_params_loop_end_label envextend_end_label in

      let allocate_new_extenv_cmd = 
        String.concat "\n" 
      ["push rbx";
      check_depth_code;
      envextend_label_code;
      envextend_loop_label_code;
      envextend_loop_end_label_code;
      envextend_params_loop_label_code;
      envextend_params_loop_end_label_code] in

      let mov_env_pointer_to_rbx = ";;; EXTEND_ENV puts the address in rbx" in

      (* Address of closure in RAX *)
      let allocate_closure = Printf.sprintf "MAKE_CLOSURE(rax, rbx, %s)\npop rbx" lcode_label in

      let jump_lcont = Printf.sprintf "jmp %s" lcont_label in

      (* All pre-body will be stored in pre_body_cmd *)
      let pre_body_cmd = 
        String.concat "\n" [allocate_new_extenv_cmd;
                            mov_env_pointer_to_rbx;
                            allocate_closure; 
                            jump_lcont] in

      (* Creating Body Label == LClosureCode *)
      let body_cmd = generate_exp consts fvars body in
      let lcode_cmd = 
lcode_label ^ ":
  push rbp
  mov rbp, rsp" ^ "
  ;;; Body Of Closure: 
  " ^  body_cmd ^ "
  ;;; End Of Body Of Closure:
  leave
  ret
  " in
      
      let closure_cmd = pre_body_cmd ^ "\n" ^ lcode_cmd ^ "\n" ^ lcont_label ^ ":\n" in
      let closure_cmd_with_comment = get_commented_cmd_string operation_description closure_cmd in
      closure_cmd_with_comment
      
    and generate_applic consts fvars proc args =
      let operation_index = get_operation_index() in  
      let operation_description = Printf.sprintf "Perform Applic#%d of: %s" operation_index (untag (Applic'(proc, args))) in 
      
      let get_arg_description index = Printf.sprintf "Argument %d of Applic statement #%d" index operation_index in
      let wrap_expr_cmd expr_index expr_cmd =
        (* Wrap an expr evaluation with useful debug information *)
        get_commented_cmd_string (get_arg_description expr_index) expr_cmd in
      let args_eval_cmds = List.map (generate_exp consts fvars) args in
      let args_eval_cmds = List.mapi wrap_expr_cmd args_eval_cmds in
      let args_eval_cmds = List.rev args_eval_cmds in (* Args should be pushed in reversed order *)
      let push_arg_cmd = "push rax ; Push argument to stack" in   
      let args_cmds = List.map (fun cmd -> cmd ^ "\n" ^ push_arg_cmd) args_eval_cmds in
      let args_cmds = String.concat "" args_cmds in

      let push_num_args_cmd = Printf.sprintf "push qword %d ; Push num of args" (List.length args) in

      let proc_cmd_description = Printf.sprintf "Evaluating proc to apply (in Applic #%d)" operation_index in
      let proc_cmd = generate_exp consts fvars proc in
      let proc_cmd = get_commented_cmd_string proc_cmd_description proc_cmd in
      (* TODO : we want to remove this; we are doing a terrible thing by throwing int3 *)
      let closure_type_verification_cmd = "; Check if RAX contains a closure\n"
      ^ "mov rsi, rax\n"
      ^ "mov bl, byte [rsi]\n"
      ^ "cmp bl, T_CLOSURE\n"
      ^ (Printf.sprintf "je ContinueApplic%d\n" operation_index) 
      ^ "int 3\n"
      ^ (Printf.sprintf "ContinueApplic%d:\n" operation_index) in
      let push_env_cmd = "push qword [rax + TYPE_SIZE] ; Push closure env" in
      let get_closure_code_cmd = "CLOSURE_CODE rax, rax ; Move closure code to rax" in
      let call_closure_code_cmd = "call rax ; Call the closure code" in
      
      let finish_applic_cmd = "
; Finished closure code. Returning from Applic #" ^ (string_of_int operation_index) ^ "
add rsp, 8*1 ; pop env
pop rbx      ; pop arg count
shl rbx, 3   ; rbx = rbx * 8
add rsp, rbx ; pop args" in

      let cmd = String.concat "\n" [args_cmds;
                                    push_num_args_cmd;
                                    proc_cmd;
                                    closure_type_verification_cmd;
                                    push_env_cmd;
                                    get_closure_code_cmd;
                                    call_closure_code_cmd;
                                    finish_applic_cmd] in
      
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
    
    and generate_applic_tp consts fvars proc args = 
      let operation_index = get_operation_index() in  
      let operation_description = Printf.sprintf "Perform ApplicTP TAIL POSITION #%d of: %s" operation_index (untag (Applic'(proc, args))) in 
      
      let get_arg_description index = Printf.sprintf "Argument %d of ApplicTP statement #%d" index operation_index in
      let wrap_expr_cmd expr_index expr_cmd =
        (* Wrap an expr evaluation with useful debug information *)
        get_commented_cmd_string (get_arg_description expr_index) expr_cmd in
      let args_eval_cmds = List.map (generate_exp consts fvars) args in
      let args_eval_cmds = List.mapi wrap_expr_cmd args_eval_cmds in
      let args_eval_cmds = List.rev args_eval_cmds in (* Args should be pushed in reversed order *)
      let push_arg_cmd = "push rax ; Push argument to stack" in   
      let args_cmds = List.map (fun cmd -> cmd ^ "\n" ^ push_arg_cmd) args_eval_cmds in
      let args_cmds = String.concat "" args_cmds in

      let push_num_args_cmd = Printf.sprintf "push qword %d ; Push num of args" (List.length args) in

      let proc_cmd_description = Printf.sprintf "Evaluating proc to apply (in ApplicTP #%d)" operation_index in
      let proc_cmd = generate_exp consts fvars proc in
      let proc_cmd = get_commented_cmd_string proc_cmd_description proc_cmd in
      (* TODO : we want to remove this; we are doing a terrible thing by throwing int3 *)
      let closure_type_verification_cmd = "; Check if RAX contains a closure\n"
      ^ "mov rsi, rax\n"
      ^ "mov bl, byte [rsi]\n"
      ^ "cmp bl, T_CLOSURE\n"
      ^ (Printf.sprintf "je ContinueApplic%d\n" operation_index) 
      ^ "int 3\n"
      ^ (Printf.sprintf "ContinueApplic%d:\n" operation_index) in
      let push_env_cmd = "push qword [rax + TYPE_SIZE] ; Push closure env" in
      let push_old_ret_addr = "push qword [rbp + 8] ; Push old return address" in
      (* do we need to save r8? *)
      let save_rbp = "mov rdi, rbp ; save current frame base pointer" in
      let save_rsp = "mov rsi, rsp ; save current frame top pointer" in
      let restore_old_frame_pointer = "pop rbp ; Restore old frame pointer" in
      let overwite_existing_frame = ";;; Overwriting Existing Frame\n"
      ^ "; set DF (Direction Flag) to 1, so addresses for the following copy instructions will decrease\n"
      ^ "std\n"
      ^ "; set the number of bytes to copy in ecx\n"
      ^ "mov rcx, rdi\n"
      ^ "sub rcx, rsi\n"
      ^ "; increase rdi to point to the base of the previous frame\n"
      ^ "mov rdi, qword [rdi]\n"
      ^ "; increase rsi to point to the top of the current data to run over with\n"
      ^ "add rsi, rcx\n"
      ^ "; the string copy instruction - copy #ecx bytes from rsi [=rsp] to rdi [=r8 =current rbp] downwards [DF flag = 1]\n"
      ^ "rep movsb\n"
      ^ "; copy the last qword manually (idk why it didn't work alone)\n"
      ^ "mov r8, qword[rsi]\n"
      ^ "mov qword[rdi], r8\n"
      ^ "; fix stack pointer\n"
      ^ "mov rsp, rdi\n"
      ^ "; fix direction flag\n"
      ^ "cld\n" in
      let jmp_to_closure_code = "CLOSURE_CODE rax, rax ; Move closure code to rax\n"
      ^ "; jmp and not call\n"
      ^ "jmp rax" in

      let cmd = String.concat "\n" [args_cmds;
                                    push_num_args_cmd;
                                    proc_cmd;
                                    closure_type_verification_cmd;
                                    push_env_cmd;
                                    push_old_ret_addr;
                                    save_rbp;
                                    save_rsp;
                                    restore_old_frame_pointer;
                                    overwite_existing_frame;
                                    jmp_to_closure_code] in
      let cmd_with_comment = get_commented_cmd_string operation_description cmd in
      cmd_with_comment
      
      
      (* Entry point *)
        in
      generate_exp consts fvars e

end;;

(* TODO: Remove. only here to make life easier *)
open Reader;;
open Tag_Parser;;
open Semantics;;
open Code_Gen;;