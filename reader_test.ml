#use "reader.ml";;

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
    | Bool(true) -> "#t"
    | Bool(false) -> "#f"
    | Nil -> "()"
    | Number(n) -> unread_number n
    | Char(c) -> unread_char c
    | String(s) -> Printf.sprintf "\"%s\"" s
    | Symbol(s) -> s
    | Pair(car, cdr) -> Printf.sprintf "(%s . %s)" (unread car) (unread cdr)

let print_sexpr s = 
    let s = unread s in
    Printf.printf "%s\n" s;;