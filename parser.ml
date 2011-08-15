open Backpack

type string_input = {pos: int; string: string}

type stream_input = {spos: int; buffer: Buffer.t; stream: char Stream.t}

type lazy_list_input = char LazyList.t

type input =
    | StringInput of string_input
    | StreamInput of stream_input
    | LazyListInput of lazy_list_input

type 'a t = input -> ('a * input) LazyList.t

exception Error of string * input

let input_of_string s = StringInput {pos = 0; string = s}

let input_of_stream n s =
    StreamInput {spos = 0; buffer = Buffer.create n; stream = s}

let input_of_lazy_list l = LazyListInput l

let empty_string input = String.length input.string = input.pos

let rec empty_stream input =
    empty_buffer input.buffer input.spos && empty_stream' input.stream
and empty_buffer b p = Buffer.length b = 0 || Buffer.length b = p
and empty_stream' s =
    try Stream.empty s = () with
    | Stream.Failure -> false

let empty_lazy_list input = Lazy.force input = LazyList.Nil

let empty = function
    | StringInput input   -> empty_string input
    | StreamInput input   -> empty_stream input
    | LazyListInput input -> empty_lazy_list input

let peek_string input =
    if empty_string input then None
    else Some (input.string.[input.pos], StringInput input)

let peek_stream input =
    let rec peek_stream' = function
        | {spos = p; buffer = b; stream = s} when empty_buffer b p ->
                let c = Stream.next s in
                Buffer.add_char b c;
                peek_stream' input
        | {spos = p; buffer = b} ->
                (Buffer.nth b p, StreamInput input)
    in
    if empty_stream input then None
    else Some (peek_stream' input)

let peek_lazy_list input =
    match Lazy.force input with
    | LazyList.Nil         -> None
    | LazyList.Cons (c, _) -> Some (c, LazyListInput input)

let peek = function
    | StringInput input   -> peek_string input
    | StreamInput input   -> peek_stream input
    | LazyListInput input -> peek_lazy_list input

let next_string input =
    match peek_string input with
    | None                        -> None
    | Some (c, StringInput input) ->
            Some (c, StringInput {input with pos = input.pos + 1})
    | _ -> invalid_arg "Parser.next_string"

let next_stream input =
    match peek_stream input with
    | None                             -> None
    | Some (c, StreamInput {spos = p}) ->
            Some (c, StreamInput {input with spos = p + 1})
    | _ -> invalid_arg "Parser.next_stream"

let next_lazy_list input =
    match Lazy.force input with
    | LazyList.Nil              -> None
    | LazyList.Cons (c, input') -> Some (c, LazyListInput input')

let next = function
    | StringInput input   -> next_string input
    | StreamInput input   -> next_stream input
    | LazyListInput input -> next_lazy_list input

let take n input =
    let rec take' b input = function
        | 0 -> Buffer.contents b
        | n ->
                match next input with
                | None ->
                        Buffer.contents b
                | Some (c, input') ->
                        Buffer.add_char b c;
                        take' b input' (n - 1)
    in take' (Buffer.create n) input n

let print_error info input =
    let next_input =
        if empty input
        then "before EOF"
        else "when parsing: \"" ^ take 10 input ^ "\""
    in
    prerr_string ("Parse error: expecting `" ^ info ^ "' " ^ next_input);
    prerr_newline ()

(* Applies parser to the input and takes the first result if there is any *)
let parse p s =
    match Lazy.force (p s) with
    | LazyList.Nil              -> None
    | LazyList.Cons ((x, _), _) -> Some x

let parse_string p s = parse p (input_of_string s)

let run_parser p s =
    try parse p s with
    | Error (info, input) ->
            print_error info input;
            None

let run_string_parser p s = run_parser p (input_of_string s)
