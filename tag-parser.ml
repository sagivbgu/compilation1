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
  
  and tag_parse_expression sexp =
  match sexp with
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
    | Pair (Symbol "cond", exps) -> tag_parse_expression (expand_cond exps)
    | Pair (Symbol "lambda", rest_of_lambda) -> tag_parse_lambda (pair_to_list_if_proper rest_of_lambda)
    | Pair (Symbol "define", rest_of_define) -> tag_parse_define rest_of_define
    | Pair (Symbol "let", rest_of_let) -> tag_parse_let rest_of_let
    | Pair (Symbol "let*", rest_of_let) -> tag_parse_let_star rest_of_let
    | Pair (Symbol "letrec", rest_of_let) -> tag_parse_letrec rest_of_let
    | Pair (Symbol "quasiquote", Pair(sexpr, Nil)) -> tag_parse_expression (expand_quasiquote sexpr)
    | Pair (Symbol "pset!", rest_of_pset) -> tag_parse_expression (expand_pset rest_of_pset)
    | Pair (func, params) -> tag_parse_applic func (pair_to_list_if_proper params)
    | Symbol(var) -> tag_parse_var var
    | x -> Printf.printf "1\n"; raise X_syntax_error

  and tag_parse_if = function
    | [if_test; if_then; if_else] -> If((tag_parse_expression if_test),
                                        (tag_parse_expression if_then),
                                        (tag_parse_expression if_else)
                                       )
    | [if_test; if_then] -> If((tag_parse_expression if_test),
                               (tag_parse_expression if_then),
                               Const(Void)
                              )
    | _ -> Printf.printf "4\n"; raise X_syntax_error

  and tag_parse_or = function
    | [] -> Const(Sexpr(Bool(false)))
    | [x] -> (tag_parse_expression x)
    | car :: cdr -> Or((List.map tag_parse_expression (car :: cdr)))

  and tag_parse_set = function
    | [exp1; exp2] -> Set ((tag_parse_expression exp1),(tag_parse_expression exp2))
    | x -> Printf.printf "15\n"; raise X_syntax_error

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
    | _ -> Printf.printf "16\n"; raise X_syntax_error

  and expand_cond = function
    | Pair(Pair(Symbol("else"), dit ), _) -> parse_cond_else_rib dit
    | Pair(Pair(cond, Pair(Symbol("=>"), Pair(func, Nil))), next) -> parse_cond_rib_2 cond func next
    | Pair(Pair(cond, dit), next) -> parse_cond_rib_1 cond dit next
    | x -> Printf.printf "17\n"; raise X_syntax_error

  and parse_cond_rib_1 cond dit next = 
    if (next = Nil)
    then (
      (Pair (Symbol "if",
      Pair (cond,
      Pair (Pair (Symbol "begin", dit), Nil))))
    )
    else (
      (Pair (Symbol "if",
      Pair (cond,
      Pair (Pair (Symbol "begin", dit),
      Pair ((expand_cond next), Nil)))))
    )

  and parse_cond_rib_2 cond func next =
    if (next = Nil)
    then (
      Pair (Symbol "let",
        Pair
        (Pair (Pair (Symbol "value", Pair (cond, Nil)),
          Pair
            (Pair (Symbol "f",
              Pair (Pair (Symbol "lambda", Pair (Nil, Pair (func, Nil))),
              Nil)),
            Nil)),
        Pair
          (Pair (Symbol "if",
            Pair (Symbol "value",
            Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)), Nil))),
          Nil)))
    )
    else (
      (Pair (Symbol "let",
        Pair
        (Pair (Pair (Symbol "value", Pair(cond, Nil)),
          Pair
            (Pair (Symbol "f",
              Pair (Pair (Symbol "lambda", Pair (Nil, Pair(func, Nil))),
              Nil)),
            Pair
            (Pair (Symbol "rest",
              Pair (Pair (Symbol "lambda", Pair (Nil, Pair((expand_cond next), Nil))),
                Nil)),
            Nil))),
        Pair
          (Pair (Symbol "if",
            Pair (Symbol "value",
            Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
              Pair (Pair (Symbol "rest", Nil), Nil)))),
          Nil)))))


  and parse_cond_else_rib dit = 
    (Pair(Symbol("begin"), dit))

  and tag_parse_lambda rest_of_lambda =
    let get_var = function Symbol(v) -> v | _ -> Printf.printf "18\n"; raise X_syntax_error
    in let parse_rest_of_lambda arglist exprs_sequence =
         match arglist with
         (* Lambda variadic *)
         | ImproperList([], Symbol(optional)) -> LambdaOpt ([], optional, exprs_sequence)
         (* Lambda optional *)
         | ImproperList(mandatory, Symbol(optional)) -> LambdaOpt ((List.map get_var mandatory), optional, exprs_sequence)
         (* Lambda simple *)
         | ProperList(mandatory) -> LambdaSimple ((List.map get_var mandatory), exprs_sequence)
         | _ -> Printf.printf "19\n"; raise X_syntax_error
    in match rest_of_lambda with
    | [] -> Printf.printf "110\n"; raise X_syntax_error
    | car :: [] -> Printf.printf "111\n"; raise X_syntax_error
    | arglist :: exprs -> parse_rest_of_lambda (pair_to_pairlist arglist) (tag_parse_seq exprs)

  and tag_parse_define s = match s with
    (*             Simple define                                    *)
    | Pair (Symbol(var), Pair (sexp, Nil)) -> Def (Var(var), (tag_parse_expression sexp))

    (*             MIT-style define:                                *)
    (*            (define (⟨var⟩ . ⟨arglist⟩) . (⟨expr⟩+))          *)
    (*      ----> (define ⟨var⟩ (lambda ⟨arglist⟩ . (⟨expr⟩+)))     *)
    | Pair (Pair (Symbol var, arglist), Pair (exprs, Nil)) ->
      (let rest_of_lambda = Pair(arglist, Pair (exprs, Nil))
       in let expansion_lambda = Pair (Pair (Symbol "lambda", rest_of_lambda), Nil)
       in let new_rest_of_define = Pair (Symbol var, expansion_lambda)
       in let expansion = Pair (Symbol "define", new_rest_of_define)       
       in (tag_parse_expression expansion))
    | _ -> Printf.printf "Error: 112\nIn Sexp: %s\n" (unread s); raise X_syntax_error

  and tag_parse_let = function
    (* (let () body) *)
    | Pair(Nil, Pair(body, Nil)) -> (
        let expansion = Pair ( Pair (Symbol "lambda", Pair (Nil, Pair (body, Nil))), Nil)
        in (tag_parse_expression expansion)
      )

    (* (let ribs ()) *)
    | Pair(ribs, Pair(Nil, Nil)) -> Printf.printf "113\n"; raise X_syntax_error

    (* (let ribs body) *)
    | Pair(ribs, body) -> (
        let split_rib cur acc = match acc, cur with 
          | ((vs, sexprs), Pair (Symbol v, Pair (sexpr, Nil))) -> (Pair(Symbol v, vs), Pair(sexpr, sexprs))
          | _ -> Printf.printf "114\n"; raise X_syntax_error
        in let ribs = (pair_to_list_if_proper ribs)
        in let split_ribs = List.fold_right split_rib ribs (Nil, Nil)        
        in let get_vs = function (vs, sexprs) -> vs
        in let get_sexprs = function (vs, sexprs) -> sexprs
        in let vs = get_vs split_ribs
        in let sexprs = get_sexprs split_ribs
        in let expansion = Pair ( Pair (Symbol "lambda", Pair (vs, body)), sexprs)
        in (tag_parse_expression expansion)
      )

    | _ -> Printf.printf "115\n"; raise X_syntax_error

  and tag_parse_let_star = function
    (* (let* () expr1 ... exprm) *)
    | Pair (Nil, sexprs) -> (tag_parse_expression (Pair (Symbol "let", Pair (Nil, sexprs))))

    (* (let* ((v Expr)) expr1 ... exprm) *)
    | Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), Nil), body) -> (
        let expansion = Pair (Symbol "let", Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), Nil), body))
        in (tag_parse_expression expansion)
      )

    (* The inductive case *)
    | Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), ribs), body) -> (
        let expansion =
          Pair (Symbol "let",
                Pair (Pair (Pair (Symbol v, Pair (vsexpr, Nil)), Nil),
                      Pair
                        (Pair (Symbol "let*",
                               Pair (ribs, body)),
                         Nil)))
        in (tag_parse_expression expansion)
      )

    | _ -> Printf.printf "116\n"; raise X_syntax_error

  and tag_parse_letrec = function
    (* (letrec ribs body) *)
    | Pair(ribs, body) -> (
        let ribs = (pair_to_list_if_proper ribs)
        in let whatever = Pair(Symbol "quote", Pair(Symbol "whatever", Nil))
        in let get_statement_from_rib = function
            | Pair(Symbol f, sexpr) -> Pair (Symbol f, Pair (whatever, Nil))
            | _ -> Printf.printf "117\n"; raise X_syntax_error
        in let get_statements cur acc = Pair ((get_statement_from_rib cur), acc)
        in let statements = List.fold_right get_statements ribs Nil
        in let make_set_f rib = Pair (Symbol "set!", rib)
        in let make_sets cur acc = Pair ((make_set_f cur), acc)
        in let sets_and_body = List.fold_right make_sets ribs body
        in let expansion = Pair (Symbol "let", Pair (statements, sets_and_body))
        in (tag_parse_expression expansion)
      )

    | _ -> Printf.printf "118\n"; raise X_syntax_error

  and tag_parse_var var = 
    if (List.mem var reserved_word_list)
    then (Printf.printf "Error:119\nIn: %s\n" var; raise X_syntax_error) 
    else (Var(var))

  and expand_quasiquote = function
    | Pair(Symbol "unquote", Pair(sexpr, Nil)) -> sexpr
    | Pair(Symbol "unquote-splicing", Pair (sexpr, Nil)) -> sexpr
        (* Pair ((Symbol "quote"), (Pair (Pair(Symbol "unquote-splicing", Pair (sexpr, Nil)), Nil))) *)
    | Nil -> Pair(Symbol "quote", Pair(Nil, Nil))
    | Symbol(sym) -> Pair(Symbol "quote", Pair(Symbol(sym), Nil))
    | Pair(car, cdr) -> expand_quasiquote_pair (Pair(car, cdr))
    (* | x -> Printf.printf "120\n%s\n" (unread x); raise X_syntax_error  *)
    | x -> x
  
  and expand_quasiquote_pair = function
    | Pair(Symbol "unquote-splicing", Pair (sexpr_car, Nil)) -> Pair (sexpr_car, Nil)
    | Pair(Pair(Symbol "unquote-splicing", Pair (sexpr_car, Nil)), sexpr_cdr) -> 
      Pair(Symbol "append", Pair (sexpr_car, Pair ((expand_quasiquote sexpr_cdr), Nil)))
    | Pair(car, cdr) -> Pair (Symbol "cons", Pair((expand_quasiquote car), Pair((expand_quasiquote cdr), Nil)))
    | Nil -> Nil
    | x -> Printf.printf "121\n"; raise X_syntax_error
  
    (* 
    
    The idea of pset:
      1) Save all the rhs exprs in a list 
      2) set! the lhs exprs respectively using that list
      
      The scheme exprs used:
      1) Building the rhs list:

          Base case:
          ((let ((lhs [rhs-sexpr]))
            (cons [rhs-sexpr] '())))
          
          New:
            (cons [rhs-sexpr] '())

          Inductive case:
            (cons [rhs-sexpr] ((lambda () <expand>)))
      
      2) The assigning function:
          
          Base case:
          (lambda () (set! [lhs-sexpr] (car <list-above>)))

          Inductive case (create a nested lambda and apply on the (cdr rhs-list)):
          (lambda () (set! [lhs-sexpr] (car <list-above>)) 
                      (set! ))
      
      Finally - create an Applic expression (func rhs-list)

      Testing:
      - in shell #use tag-parser_test.ml
      - type: untag (List.hd (p (read_sexprs "(pset! <rest_of_pset)")));;
      - in DrRacket - define the lhs-exprs
                    - enter the expression given from previous section 
                      (notice there's a BUG in untag -> empty list should be '()
                      but the output is () )
      - type the lhs expression again to see the changes
      - live long and prosper

      Simple Example:
      (define x 5)
      (pset! (x 3))
      x 
      => 3

      untag (List.hd (p (read_sexprs "(pset! (x 3))"))) =>
      ((lambda (rhs-list) (set! x (car rhs-list))) ((lambda (lhs) (cons lhs ())) 3/1))

      *)

  and expand_pset sexpr = 
    let rhs_list = expand_pset_rhs_list sexpr in
    let func = expand_pset_assign_lists sexpr in
    (Pair (func, Pair (rhs_list, Nil)))

  and expand_pset_rhs_list = function
    | Pair (Pair (lhs, Pair (rhs, Nil)), Nil) -> 
        Pair (Symbol "cons",
          Pair (rhs, Pair (Pair (Symbol "quote", Pair (Nil, Nil)), Nil)))

    | Pair (Pair (lhs, Pair (rhs, Nil)), rest) -> 
        Pair (Symbol "cons", 
          Pair (rhs, (Pair ((expand_pset_rhs_list rest), Nil))))
    | _ -> Printf.printf "122\n"; raise X_syntax_error
  
  and expand_pset_assign_lists = function
    | Pair (Pair (lhs, Pair (rhs, Nil)), Nil) -> 
      (Pair (Symbol "lambda",
        Pair (Pair (Symbol "rhs-list", Nil),
        Pair
          (Pair (Symbol "set!",
            Pair (lhs,
            Pair (Pair (Symbol "car", Pair (Symbol "rhs-list", Nil)), Nil))),
          Nil))))

    | Pair (Pair (lhs, Pair (rhs, Nil)), rest) -> 
      (Pair (Symbol "lambda",
        Pair (Pair (Symbol "rhs-list", Nil),
        Pair
          (Pair (Symbol "set!",
            Pair (lhs,
            Pair (Pair (Symbol "car", Pair (Symbol "rhs-list", Nil)), Nil))),
          Pair
          (Pair ((expand_pset_assign_lists rest),
            Pair (Pair (Symbol "cdr", Pair (Symbol "rhs-list", Nil)), Nil)),
          Nil)))))
    | _ -> Printf.printf "123\n"; raise X_syntax_error

  and tag_parse_applic func params = Applic((tag_parse_expression func), (tag_parse_exprs params))

  (* *************** TAG-PARSER ***************** *)
  let tag_parse_expressions sexpr = tag_parse_exprs sexpr;;


end;; (* struct Tag_Parser *)