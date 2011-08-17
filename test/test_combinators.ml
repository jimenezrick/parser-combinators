open Combinators

let run_test p s =
    match Parser.run_string_parser p s with
    | None   -> assert false
    | Some s -> Backpack.Str.implode s

let () =
    let p = ( >>? ) (+) integer integer in
    let s = "1   -2" in
    let r = Some (-1) in
    assert (r = Parser.run_string_parser p s)

let () =
    let p = ((char '-' >>:: nat) <|> neg) >>@ word' in
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
    let p  = opt nat >>@ word' in
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
    let s2 = "1  +2  -3" in
    let s3 = "-1+2" in
    let s4 = "-1  +2*3" in
    let r1 = Some 1 in
    let r2 = Some 0 in
    let r3 = Some 1 in
    let r4 = Some 3 in
    assert (r1 = Parser.run_string_parser p s1);
    assert (r2 = Parser.run_string_parser p s2);
    assert (r3 = Parser.run_string_parser p s3);
    assert (r4 = Parser.run_string_parser p s4)

let () =
    let p  = chainr1 integer arith_op in
    let s1 = "1" in
    let s2 = "1+2-3" in
    let s3 = "-1  +2" in
    let s4 = "-1+2*3" in
    let r1 = Some 1 in
    let r2 = Some 0 in
    let r3 = Some 1 in
    let r4 = Some 5 in
    assert (r1 = Parser.run_string_parser p s1);
    assert (r2 = Parser.run_string_parser p s2);
    assert (r3 = Parser.run_string_parser p s3);
    assert (r4 = Parser.run_string_parser p s4)

let () =
    let p  = chainl integer arith_op 0 in
    let s1 = "" in
    let s2 = "-1+2*3" in
    let r1 = Some 0 in
    let r2 = Some 3 in
    assert (r1 = Parser.run_string_parser p s1);
    assert (r2 = Parser.run_string_parser p s2)

let () =
    let p  = chainr integer arith_op 0 in
    let s1 = "" in
    let s2 = "-1+2*3" in
    let r1 = Some 0 in
    let r2 = Some 5 in
    assert (r1 = Parser.run_string_parser p s1);
    assert (r2 = Parser.run_string_parser p s2)

let () =
    let p = choice [string "x"; nat] in
    let s = "666" in
    assert (s = run_test p s)

let () =
    let p1 = choice  [string "x"; nat] in
    let p2 = choice1 [string "y"; neg] in
    let p  = p1 >>@ p2 in
    let s  = "1-2" in
    assert (s = run_test p s)

let () =
    let p  = (skip_many (char '.')) >>@ nat in
    let s1 = "123" in
    let s2 = "...123" in
    let r  = "123" in
    assert (r = run_test p s1);
    assert (r = run_test p s2)

let () =
    let p = (skip_many1 (char '.')) >>@ nat in
    let s = "...123" in
    let r = "123" in
    assert (r = run_test p s)

let () =
    let p  = end_by digit (char ';') in
    let s1 = "" in
    let s2 = "1;2;3;" in
    let r1 = "" in
    let r2 = "123" in
    assert (r1 = run_test p s1);
    assert (r2 = run_test p s2)

let () =
    let p = end_by1 digit (char ';') in
    let s = "1;2;3;" in
    let r = "123" in
    assert (r = run_test p s)

let () =
    let p = drop (string "/*") >> many_till any (string "*/") in
    let s = "/* 123 */" in
    let r = " 123 " in
    assert (r = run_test p s)

let () =
    let p = many1 any >>@ not_followed_by (( = ) ';') in
    let s = "AB6;" in
    let r = "AB" in
    assert (r = run_test p s)

let () =
    let p = junk >> many word >>@ (keyword "end" >>:: mzero) in
    let s = "  1  2   3  end" in
    let r = ["1"; "2"; "3"; "end"] in
    assert (Some (List.map Backpack.Str.explode r) = Parser.run_string_parser p s)












let () =
    let p = count 3 digit in
    let s = "1234" in
    let r = "123" in
    assert (r = run_test p s)

let () =
    let p = token nat >>@ token neg in
    let s = "123  -456" in
    let r = "123-456" in
    assert (r = run_test p s)

let () =
    let p = token nat >>@ return [';'] >>@ many any >>@ token neg in
    let s = "123-456" in
    let r = "123;-456" in
    assert (r = run_test p s)

let () =
    let p = keyword "var" >>@ word' in
    let s = "var  i" in
    let r = "vari" in
    assert (r = run_test p s)

let () =
    let p = (char '1' >>:: eof) ||| (char '1' ^>>:: char '2' ^>>:: eof) in
    let s = "12" in
    let r = "12" in
    assert (r = run_test p s)

let () =
    let p =
        junk_start ((keyword "foo" >>@ junk_eof) |||
                    (keyword "foo" >>@ keyword "bar" >>@ junk_eof)) in
    let s = "   foo   bar   " in
    let r = "foobar" in
    assert (r = run_test p s)

let () =
    let p = many1 integer in
    let s = "1  2   3" in
    let r = Some [1; 2; 3] in
    assert (r = Parser.run_string_parser p s)

let () =
    print_string "*** All tests passed ***";
    print_newline ()
