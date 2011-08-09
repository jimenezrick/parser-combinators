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
    let p = opt (char 'x' >>:: mzero) >>@ (char 'o' >>:: mzero) in
    let s1 = "o" in
    let s2 = "xo" in
    assert (s1 = run_test p s1);
    assert (s2 = run_test p s2)

let () =
    let p = opt nat >>@ word in
    let s1 = "123abc" in
    let s2 = "abc" in
    assert (s1 = run_test p s1);
    assert (s2 = run_test p s2)






let () =
    print_string "All tests passed";
    print_newline ()
