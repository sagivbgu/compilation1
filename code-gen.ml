#use "semantic-analyser.ml";;

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
      (* TODO: Should we ignore the parameters? i think we should *)
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
      let cmd = Printf.sprintf "MAKE_LITERAL_CHAR('%c')" ch in
      let cmd = make_row_cmd_with_comment cmd offset (Printf.sprintf "'%c'" ch) in
      (Sexpr(Char(ch)), (offset, cmd)) in
    
    let make_str_row offset str = 
      let cmd = Printf.sprintf "MAKE_LITERAL_STRING(\"%s\")" str in
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

  (* let make_fvars_tbl asts = raise X_not_yet_implemented;; *)
  let make_fvars_tbl asts = [("", 0)];;
  
  let generate consts fvars e = 
    (* mov rax,AddressInConstTable(c) *)
    let generate_const const consts_tbl = 
      let index_and_cmd = List.assoc const consts_tbl in
      let extract_index (i, c) = i in
      let index = extract_index index_and_cmd in
      let cmd = Printf.sprintf "mov rax, const_tbl+%d" index in
      let cmd_with_comment = Printf.sprintf "%s ; mov %s to rax" cmd (untag (Const'(const))) in
      cmd_with_comment in

    let rec generate_exp consts fvars exp = match exp with
      | Const'(const) -> generate_const const consts
      | _ -> "" in
    
    (* Entry point *)
    generate_exp consts fvars e

end;;

(* TODO: Remove. only here to make life easier *)
open Reader;;
open Tag_Parser;;
open Semantics;;
open Code_Gen;;