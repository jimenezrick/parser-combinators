open Parser_combinators

(*
 * LET  ::= "let" VAR "=" EXPR ["in" EXPR]
 * VAR  ::= <string>
 * EXPR ::= <int>
 *)

type let_construct =
    | Var of string
    | Expr of expr

type expr_construct =
    | Int of int

let input =





let var = (alphanum ||| char '_') >>:: word

let parser = symbol "let" >> var >>:: symbol "=" expr







let () =
    let p = nat >>@ neg >>@ word in
    match run_parser parser input with
    | None   -> print_string "Error: []"; print_newline ()
    | Some s -> print_string (Backpack.implode s); print_newline ()
