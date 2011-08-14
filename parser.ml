type string_input = {pos: int; string: string}

type stream_input = {pos: int; buffer: Buffer.t; stream: char Stream.t}

type lazy_list_input = char Backpack.LazyList.t

type input =
    | StringInput of string_input
    | StreamInput of stream_input
    | LazyListInput of lazy_list_input

type 'a t = input -> ('a * input) list

exception Error of string * input

let input_of_string s = StringInput {pos = 0; string = s}

let input_of_stream n s = StreamInput {pos = 0; buffer = Buffer.create n; stream = s}

let input_of_lazy_list l = LazyListInput l





let empty = function
    | StringInput input   -> empty_string input
    | StreamInput input   -> empty_stream input
    | LazyListInput input -> empty_lazy_list input

let empty_string input =
    String.length input.string = input.pos

let empty_stream input =
    Buffer.empty input.buffer && Stream.empty input.stream ||
    Buffer.length input.buffer = input.pos && Stream.empty input.stream

let empty_lazy_list input =
    Backpack.LazyList.force input = None







let peek input =
    if empty input then None
    else
        match input with
        | StringInput input   -> Some (peek_string input)
        | StreamInput input   -> Some (peek_stream input)
        | LazyListInput input -> Some (peek_lazy_list input)

let peek_string input = (input.string.[input.pos], input)

let peek_stream = function
    | {buffer = b; stream = s} as input when Buffer.empty b ->
            (* FIXME: Si se lee del Stream, lo metemos luego en el buffer! *)
            let Some c = Stream.peek s in
            (c, input)
    | {buffer = b} as input ->
            (Buffer.nth b 0, input)

let peek_lazy_list l =
    let Some (h, t) = Backpack.force l in
    (h, l)














let next input =
    if empty input then None
    else
        match input with
        | StringInput input   -> Some (next_string input)
        | StreamInput input   -> Some (next_stream input)
        | LazyListInput input -> Some (next_lazy_list input)

let next_string input =
    match peek input with
    | None            -> None
    | Some (c, input) -> Some (c, {input with pos = input.pos + 1})





(* TODO: miro aqui directamente si el Stream esta vacio??? *)
let next_stream = function
    let read_stream input =
        let c = Stream.next input.stream in
        (c, {input with pos = pos + 1; buffer = Buffer.add_char b})



    | {buffer = b; stream = s} as input when Buffer.empty b ->
            read_stream input


    | {pos = p; buffer = b} as input when Buffer.length b = p ->
            read_stream input


    | {pos = p; buffer = b} as input ->
            (Buffer.nth b p, {input with pos = pos + 1})









(* TODO TODO TODO *)
let next_lazy_list input = failwith "Not implemented"
(* TODO TODO TODO *)












(* FIXME FIXME: no usar directamente input.string *)
(* FIXME FIXME: no usar directamente input.string *)
(* FIXME FIXME: no usar directamente input.string *)
let take_next_chars input n =
    let len  = String.length input.string - input.pos in
    if n > len
    then String.sub input.string input.pos len
    else String.sub input.string input.pos n
(* FIXME FIXME FIXME *)
(* FIXME FIXME FIXME *)
(* FIXME FIXME FIXME *)





(* Applies parser to the input and takes the first result if there is any *)
let parse p s =
    match p (input_of_string s) with
    | []          -> None
    | (x, _) :: _ -> Some x

let print_error info input =
    let next =
        if empty input then "before EOF"
        else "when parsing: \"" ^ take_next_chars input 10 ^ "\""
    in
    prerr_string ("Parse error: expecting `" ^ info ^ "' " ^ next);
    prerr_newline ();
    None

let run_parser p s =
    try parse p s with
    | Error (info, input) ->
            print_error info input
