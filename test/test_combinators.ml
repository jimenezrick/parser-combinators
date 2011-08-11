open Parser_combinators

let run_test p s =
    match run_parser p s with
    | None   -> assert false
    | Some s -> Backpack.implode s

let () =
    let p = ((char '-' >>:: nat) <|> neg) >>@ word in
    let s = "-123word" in
    assert (s = run_test p s)

let () =
    let p1 = many  (char 'x') in
    let p2 = many1 (char 'x') in
    let s1 = "" in
    let s2 = "x" in
    assert (s1 = run_test p1 s1);
    assert (s2 = run_test p1 s2);
    assert (s2 = run_test p2 s2)

let () =
    let p  = opt (char 'x' >>:: mzero) >>@ (char 'o' >>:: mzero) in
    let s1 = "o" in
    let s2 = "xo" in
    assert (s1 = run_test p s1);
    assert (s2 = run_test p s2)

let () =
    let p  = opt nat >>@ word in
    let s1 = "123abc" in
    let s2 = "abc" in
    assert (s1 = run_test p s1);
    assert (s2 = run_test p s2)

let () =
    let p = drop (char 'x' >>:: mzero) >>@ (char 'o' >>:: mzero) in
    let s = "xo" in
    let r = "o" in
    assert (r = run_test p s)

let () =
    let p = drop (string "x") >>@ string "o" in
    let s = "xo" in
    let r = "o" in
    assert (r = run_test p s)

let () =
    let p   = drop_opt (string "x") >>@ string "o" in
    let s1  = "xo" in
    let s2  = "o" in
    assert (s2 = run_test p s1);
    assert (s2 = run_test p s2)

let () =
    let p  = sep_by1 digit (char '.') in
    let s1 = "1" in
    let s2 = "1.2.3" in
    let r2 = "123" in
    assert (s1 = run_test p s1);
    assert (r2 = run_test p s2)

let () =
    let p  = sep_by1 digit (string ".") in
    let s1 = "1" in
    let s2 = "1.2.3" in
    let r2 = "123" in
    assert (s1 = run_test p s1);
    assert (r2 = run_test p s2)

let () =
    let p  = sep_by digit (string ".") in
    let s1 = "" in
    let s2 = "1.2.3" in
    let r2 = "123" in
    assert (s1 = run_test p s1);
    assert (r2 = run_test p s2)

let () =
    let p = between (string "[") (string "x") (string "]") in
    let s = "[x]" in
    let r = "x" in
    assert (r = run_test p s)

let () =
    let p = between (string "[") nat (string "]") in
    let s = "[666]" in
    let r = "666" in
    assert (r = run_test p s)

let () =
    let p  = chainl1 integer arith_op in
    let s1 = "1" in
    let s2 = "1+2-3" in
    let s3 = "-1+2" in
    let s4 = "-1+2*3" in
    let r1 = Some 1 in
    let r2 = Some 0 in
    let r3 = Some 1 in
    let r4 = Some 3 in
    assert (r1 = run_parser p s1);
    assert (r2 = run_parser p s2);
    assert (r3 = run_parser p s3);
    assert (r4 = run_parser p s4)

let () =
    let p  = chainr1 integer arith_op in
    let s1 = "1" in
    let s2 = "1+2-3" in
    let s3 = "-1+2" in
    let s4 = "-1+2*3" in
    let r1 = Some 1 in
    let r2 = Some 0 in
    let r3 = Some 1 in
    let r4 = Some 5 in
    assert (r1 = run_parser p s1);
    assert (r2 = run_parser p s2);
    assert (r3 = run_parser p s3);
    assert (r4 = run_parser p s4)

let () =
    let p  = chainl integer arith_op 0 in
    let s1 = "" in
    let s2 = "-1+2*3" in
    let r1 = Some 0 in
    let r2 = Some 3 in
    assert (r1 = run_parser p s1);
    assert (r2 = run_parser p s2)

let () =
    let p  = chainr integer arith_op 0 in
    let s1 = "" in
    let s2 = "-1+2*3" in
    let r1 = Some 0 in
    let r2 = Some 5 in
    assert (r1 = run_parser p s1);
    assert (r2 = run_parser p s2)









(*
let () =
    let p =
    let s =
    assert (s = run_test p s)
*)


let () =
    print_string "All tests passed";
    print_newline ()