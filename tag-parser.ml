#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                          (expr_eq th1 th2) &&
                                          (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                           (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
    (List.for_all2 String.equal vars1 vars2) &&
    (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
    (String.equal var1 var2) &&
    (List.for_all2 String.equal vars1 vars2) &&
    (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
    (expr_eq e1 e2) &&
    (List.for_all2 expr_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

  let reserved_word_list =
    ["and"; "begin"; "cond"; "define"; "else";
     "if"; "lambda"; "let"; "let*"; "letrec"; "or";
     "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
     "unquote-splicing"];;  

  (* work on the tag parser starts here *)

  (* *************** UNREAD ***************** *)
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

  (* *************** UTILS ***************** *)
  type 'a pairlist = 
    | ProperList of 'a list
    | ImproperList of 'a list * 'a;; (* A tuple of the form: (list_except_last_item, last_item) *)

  let rec pair_to_pairlist = function
    | Nil -> ProperList([])
    | Pair(car, cdr) -> (
        let cdr_pairlist = pair_to_pairlist cdr
        in let append_to_pairlist car cdr_pairlist =
             match cdr_pairlist with
             | ProperList(lst) -> ProperList(car :: lst)
             | ImproperList(lst, last) -> ImproperList(car :: lst, last)
        in append_to_pairlist car cdr_pairlist)
    | last -> ImproperList([], last)

  let pair_to_list_if_proper pair =
    let lst = (pair_to_pairlist pair)
    in let get_list_if_proper = function ProperList(lst) -> lst | ImproperList(_) -> raise X_this_should_not_happen
    in (get_list_if_proper lst)

  (* *************** EXPR ***************** *)
  let rec tag_parse_exprs sexprs = List.map tag_parse_expression sexprs

  and tag_parse_expression = function
    | Bool(b) -> Const(Sexpr(Bool(b)))
    | Char(c) -> Const(Sexpr(Char(c)))
    | Number(n) -> Const(Sexpr(Number(n)))
    | String(s) -> Const(Sexpr(String(s)))
    | Pair (Symbol "quote", Pair(sexpr, Nil)) -> Const(Sexpr(sexpr)) (* Intentionally not recursive *)
    | Pair (Symbol "if", rest_of_if) -> tag_parse_if (pair_to_list_if_proper rest_of_if)
    | Pair (Symbol "or", exps) -> tag_parse_or (pair_to_list_if_proper exps)
    | Pair (Symbol "set!", exps) -> tag_parse_set (pair_to_list_if_proper exps)
    | Pair (Symbol "begin", exps) -> tag_parse_seq (pair_to_list_if_proper exps)
    | Pair (Symbol "and", exps) -> tag_parse_and exps
    | Pair (Symbol "cond", exps) -> tag_parse_cond exps
    | Pair (Symbol "lambda", rest_of_lambda) -> tag_parse_lambda (pair_to_list_if_proper rest_of_lambda)
    | Pair (Symbol "define", rest_of_define) -> tag_parse_define rest_of_define
    | Pair (Symbol "let", rest_of_let) -> tag_parse_let rest_of_let
    | Pair (Symbol "let*", rest_of_let) -> tag_parse_let_star rest_of_let
    | Pair (Symbol "letrec", rest_of_let) -> tag_parse_letrec rest_of_let
    | Pair (func, params) -> tag_parse_applic func (pair_to_list_if_proper params)
    | Symbol(var) -> tag_parse_var var
    | x -> Printf.printf "\n%s\n" (unread x); raise X_not_yet_implemented

  and tag_parse_if = function
    | [if_test; if_then; if_else] -> If((tag_parse_expression if_test),
                                        (tag_parse_expression if_then),
                                        (tag_parse_expression if_else)
                                       )
    | [if_test; if_then] -> If((tag_parse_expression if_test),
                               (tag_parse_expression if_then),
                               Const(Void)
                              )
    | _ -> raise X_syntax_error

  and tag_parse_or = function
    | [] -> Or([(tag_parse_expression (Bool(false)))])
    | lst -> Or((List.map tag_parse_expression lst))

  and tag_parse_set = function
    | [exp1; exp2] -> Set ((tag_parse_expression exp1),(tag_parse_expression exp2))
    | x -> raise X_syntax_error

  (* Seq needs to be flatten -> Seq([x ; Seq(y)]) => Seq([x; y]) *)
  and tag_parse_seq = function
    | [] -> (Const Void)
    | [sexp] -> (tag_parse_expression sexp)
    | sexps -> Seq( (List.flatten (List.map flatten_seq (List.map tag_parse_expression sexps))) )

  and flatten_seq = function
    | Seq(x) -> x
    | x -> [x]


  and tag_parse_and = function
    | Nil -> (tag_parse_expression (Bool(true)))
    | Pair(first, Nil) -> (tag_parse_expression first)
    | Pair(first, rest) -> (tag_parse_if [first ; Pair(Symbol("and"), rest) ; Bool(false)])
    | _ -> raise X_syntax_error

  and tag_parse_cond = function
    | Pair(Pair(Symbol("else"), dit ), _) -> parse_cond_else_rib dit
    | Pair(Pair(cond, Pair(Symbol("=>"), func)), next) -> parse_cond_rib_2 cond func next
    | Pair(Pair(cond, dit), next) -> parse_cond_rib_1 cond dit next
    | _ -> raise X_syntax_error

  and parse_cond_rib_1 cond dit next = 
    If((tag_parse_expression cond), 
       (tag_parse_expression (Pair(Symbol("begin"), dit))), 
       (tag_parse_cond next))

  and parse_cond_rib_2 cond func next =
    let expanded = 
      Pair (Symbol "let",
            Pair
              (Pair (Pair (Symbol "value", Pair (cond, Nil)),
                     Pair
                       (Pair (Symbol "f",
                              Pair (Pair (Symbol "lambda", Pair (Nil, Pair (func, Nil))),
                                    Nil)),
                        Pair
                          (Pair (Symbol "rest",
                                 Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair(Symbol "cond", next), Nil))),
                                       Nil)),
                           Nil))),
               Pair
                 (Pair (Symbol "if",
                        Pair (Symbol "value",
                              Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
                                    Pair (Pair (Symbol "rest", Nil), Nil)))),
                  Nil))) in
    (tag_parse_expression expanded)

  and parse_cond_else_rib dit = 
    tag_parse_if [(Bool(true)); (Pair(Symbol("begin"), dit))]

  and tag_parse_lambda rest_of_lambda =
    let get_var = function Symbol(v) -> v | _ -> raise X_syntax_error
    in let parse_rest_of_lambda arglist exprs_sequence =
         match arglist with
         (* Lambda variadic *)
         | ImproperList([], Symbol(optional)) -> LambdaOpt ([], optional, exprs_sequence)
         (* Lambda optional *)
         | ImproperList(mandatory, Symbol(optional)) -> LambdaOpt ((List.map get_var mandatory), optional, exprs_sequence)
         (* Lambda simple *)
         | ProperList(mandatory) -> LambdaSimple ((List.map get_var mandatory), exprs_sequence)
         | _ -> raise X_syntax_error
    in match rest_of_lambda with
    | [] -> raise X_syntax_error
    | car :: [] -> raise X_syntax_error
    | arglist :: exprs -> parse_rest_of_lambda (pair_to_pairlist arglist) (tag_parse_seq exprs)

  and tag_parse_define = function
    (*             Simple define                                    *)
    | Pair (Symbol(var), Pair (sexp, Nil)) -> Def (Var(var), (tag_parse_expression (Pair(sexp, Nil))))


    (*             MIT-style define:                                *)
    (*            (define (⟨var⟩ . ⟨arglist⟩) . (⟨expr⟩+))          *)
    (*      ----> (define ⟨var⟩ (lambda ⟨arglist⟩ . (⟨expr⟩+)))     *)
    | Pair (Pair (Symbol var, arglist), Pair (exprs, Nil)) ->
      (let rest_of_lambda = Pair(arglist, exprs)
       in let expansion_lambda = Pair (Symbol "lambda", rest_of_lambda)
       in (tag_parse_expression (Pair (Symbol "define", expansion_lambda))))
    | _ -> raise X_syntax_error

  and tag_parse_let = function
    (* (let () body) *)
    | Pair(Nil, Pair(body, Nil)) -> (
        let expansion = Pair ( Pair (Symbol "lambda", Pair (Nil, Pair (body, Nil))), Nil)
        in (tag_parse_expression expansion)
      )

    (* (let (rib . ribs) ()) *)
    | Pair(Pair(rib, ribs), Pair(Nil, Nil)) -> raise X_syntax_error

    (* (let (rib . ribs) body) *)
    | Pair(Pair(rib, ribs), Pair(body, Nil)) -> (
        let split_rib acc cur = match acc, cur with 
          | ((vs, sexprs), Pair (Symbol v, Pair (sexpr, Nil))) -> (Pair(Symbol v, vs), Pair(sexpr, sexprs))
          | _ -> raise X_syntax_error
        in let ribs = (pair_to_list_if_proper ribs)
        in let split_ribs = List.fold_left split_rib (Nil, Nil) (rib :: ribs)
        in let get_vs = function (vs, sexprs) -> vs
        in let get_sexprs = function (vs, sexprs) -> sexprs
        in let vs = get_vs split_ribs
        in let sexprs = get_sexprs split_ribs
        in let expansion = Pair ( Pair (Symbol "lambda", Pair (vs, Pair (body, Nil))), Pair (sexprs, Nil))
        in (tag_parse_expression expansion)
      )

    | _ -> raise X_syntax_error

  and tag_parse_let_star = function
    (* (let* () expr1 ... exprm) *)
    | Pair (Nil, sexprs) -> (tag_parse_expression (Pair (Symbol "let", Pair (Nil, sexprs))))

    (* (let* ((v Expr)) expr1 ... exprm) *)
    | Pair (Pair (Symbol v, Pair (sexpr, Nil)), sexprs) ->
      (tag_parse_expression (Pair (Symbol "let", Pair (Pair (Symbol v, Pair (sexpr, Nil)), sexprs))))

    (* The inductive case *)
    | Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), ribs), Pair(body, Nil)) -> (
        let expansion =
          Pair (Symbol "let",
                Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), Nil),
                      Pair
                        (Pair (Symbol "let*",
                               Pair (ribs, body)),
                         Nil)))
        in (tag_parse_expression expansion)
      )

    | _ -> raise X_syntax_error

  and tag_parse_letrec = function
    (* (letrec ribs body) *)
    | Pair(ribs, body) -> (
        let ribs = (pair_to_list_if_proper ribs)
        in let get_statement_from_rib = function 
            | Pair(Symbol f, sexpr) -> Pair (Symbol f, Bool false) (* Instead of 'whatever *)
            | _ -> raise X_syntax_error
        in let get_statements acc cur = Pair ((get_statement_from_rib cur), acc)
        in let statements = List.fold_right get_statements ribs Nil
        in let make_set_f rib = Pair (Symbol "set!", rib)
        in let make_sets acc cur = Pair ((make_set_f cur), acc)
        in let sets_and_body = List.fold_right make_sets ribs body
        in let expansion = Pair (Symbol "let", Pair (statements, sets_and_body))
        in (tag_parse_expression expansion)
      )

    | _ -> raise X_syntax_error

  and tag_parse_var var = 
    if (List.mem var reserved_word_list)
    then (raise X_syntax_error) 
    else (Var(var))


  and tag_parse_applic func params = Applic((tag_parse_expression func), (tag_parse_exprs params))

  (* *************** TAG-PARSER ***************** *)
  let tag_parse_expressions sexpr = tag_parse_exprs sexpr;;


end;; (* struct Tag_Parser *)

