let rec consume_input input =
    match Parser.next input with
    | None ->
            assert (Parser.empty input);
            []
    | Some (c, input') ->
            assert (not (Parser.empty input));
            match Parser.peek input with
            | None ->
                    invalid_arg "Test_parser.consume_input"
            | Some (c', _) ->
                    assert (c = c');
                    c :: consume_input input'

let test_input input expected =
    match consume_input input with
    | []  -> ()
    | res -> assert (Backpack.Str.implode res = expected)

let () =
    let s      = "0123456789" in
    let input1 = Parser.input_of_string s in
    let input2 = Parser.input_of_stream 10 (Stream.of_string s) in
    let input3 = Parser.input_of_lazy_list (Backpack.LazyList.of_string s) in
    List.iter (fun i -> test_input i s) [input1; input2; input3]

let () =
    print_string "*** All tests passed ***";
    print_newline ()
