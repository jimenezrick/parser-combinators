(*
 * Simple monadic parser combinators library:
 *
 * Pure functional joy
 *)

type input = {cursor: int; text: string}

type 'a t = input -> ('a * input) list

exception Error of string * input

let input_of_string s = {cursor = 0; text = s}

let empty input =
    let len = String.length input.text in
    match input with
    | {cursor = l} when l = len -> true
    | _                         -> false

let peek input =
    if empty input
    then None
    else Some (input.text.[input.cursor], input)

let next input =
    match peek input with
    | None            -> None
    | Some (c, input) -> Some (c, {input with cursor = input.cursor + 1})

let scan =
    fun input ->
        match peek input with
        | None   -> []
        | Some x -> [x]

let item =
    fun input ->
        match next input with
        | None   -> []
        | Some x -> [x]

let return x = fun input -> [(x, input)]

let fail = fun input -> []

let mzero = return []

let ( >>= ) p f =
    fun input ->
        List.concat (List.map (fun (x, input') -> f x input') (p input))

let ( >> ) p q = p >>= fun _ -> q

let ( >>? ) f p q =
    p >>= fun x ->
    q >>= fun y ->
    return (f x y)

let ( >>:: ) p q =
    let cons = fun x xs -> x :: xs in
    ( >>? ) cons p q

let ( >>@ ) p q = ( >>? ) ( @ ) p q

let ( ||| ) p q =
    fun input ->
        match p input with
        | [] -> q input
        | x  -> x

(* Non-deterministic alternative operator, tries all possibilities *)
let ( <|> ) p q = fun input -> p input @ q input

let ( <?> ) p info = p ||| fun input -> raise (Error (info, input))

let rec many p =
    let continue =
        p      >>= fun x  ->
        many p >>= fun xs ->
        return (x :: xs)
    in continue ||| mzero

let many1 p = p >>:: many p

let opt p = p ||| mzero

let drop p = p >> mzero

let drop_opt p = drop p ||| mzero

let sep_by1 p sep = p >>:: many (drop sep >> p)

let sep_by p sep = sep_by1 p sep ||| mzero

let between o p c = drop o >>@ p >>@ drop c

let chainl1 p op =
    let rec rest x =
        (op >>= fun f ->
         p  >>= fun y ->
         rest (f x y)) ||| return x
    in p >>= rest

let rec chainr1 p op =
    let rest x =
        (op           >>= fun f ->
         chainr1 p op >>= fun y ->
         return (f x y)) ||| return x
    in p >>= rest

let chainl p op x = chainl1 p op ||| return x

let chainr p op x = chainr1 p op ||| return x

let choice ps = List.fold_right ( <|> ) ps mzero

let choice1 ps = List.fold_right ( ||| ) ps mzero

(*
 * If `skip_many' were defined as:
 *
 *     let rec skip_many p = p >> skip_many p ||| mzero
 *
 * It would cause an infinite loop because first `p' is evaluated, then
 * `skip_many p' and finally `>>'. So if `p' fails, `skip_many p' is evaluated
 * no matter what giving an infinite loop before we can stop with `>>'.
 *
 * The definition of `many' solves this problem using the operator `>>='
 * directly creating some lambdas. Therefore, the evaluation of a lambda body
 * is suspended until it is needed.
 *)
let skip_many p = drop (many p)

let skip_many1 p = drop (many1 p)

let end_by p sep = many (p >>= fun x -> sep >> return x)

let end_by1 p sep = many1 (p >>= fun x -> sep >> return x)

let many_till p until = many p >>:: drop until

let not_followed_by p not_q = (not_q >> fail) ||| p

let rec count n p =
    match n with
    | n when n < 0 -> fail
    | 0            -> mzero
    | n            -> p >>:: count (n - 1) p

let pred p =
    item >>= fun x ->
        if p x then return x else fail

let any = pred (fun _ -> true)

let char c = pred (fun c' -> c = c')

let digit = pred (fun c -> '0' <= c && c <= '9')

let lower = pred (fun c -> 'a' <= c && c <= 'z')

let upper = pred (fun c -> 'A' <= c && c <= 'Z')

let letter = lower ||| upper

let alphanum = letter ||| digit

let word = many1 (alphanum ||| char '_')

let nat = many1 digit

let neg = char '-' >>:: nat

let int = neg ||| nat

let space = (char ' ' ||| char '\t') >>:: mzero

let eol = (char '\n' >>:: mzero) ||| (char '\r' >>:: (char '\n' >>:: mzero))

let sep = space ||| eol

let junk = many sep

let token p = junk >> p

let integer_of_token p =
    p >>= fun x ->
    return (int_of_string (Backpack.Str.implode x))

let natural = integer_of_token nat

let negative = integer_of_token neg

let integer = negative ||| natural

let string s =
    let rec string' = function
        | []      -> mzero
        | c :: cs -> char c >>:: string' cs
    in string' (Backpack.Str.explode s)

let symbol s = token (string s)

let eof =
    fun input ->
        if empty input
        then mzero input
        else fail input

let junk_eof = junk >> eof

let arith_op =
    (char '+' >> return ( + )) |||
    (char '-' >> return ( - )) |||
    (char '*' >> return ( * )) |||
    (char '/' >> return ( / ))

let take_next_chars input n =
    let len  = String.length input.text - input.cursor in
    if n > len
    then String.sub input.text input.cursor len
    else String.sub input.text input.cursor n

(* Applies parser to the input and takes the first result if there is any *)
let parse p s =
    match p (input_of_string s) with
    | []          -> None
    | (x, _) :: _ -> Some x

let print_error info input =
    let next =
        if empty input
        then "before EOF"
        else "when parsing: \"" ^ take_next_chars input 10 ^ "\""
    in
    prerr_string ("Parse error: expecting `" ^ info ^ "' " ^ next);
    prerr_newline ();
    None

let run_parser p s =
    try parse p s with
    | Error (info, input) ->
            print_error info input
