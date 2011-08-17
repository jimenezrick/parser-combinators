open Backpack

let scan =
    fun input ->
        match Parser.peek input with
        | None   -> LazyList.empty
        | Some x -> LazyList.create x

let item =
    fun input ->
        match Parser.next input with
        | None   -> LazyList.empty
        | Some x -> LazyList.create x

let return x = fun input -> LazyList.create (x, input)

let fail = fun input -> LazyList.empty

let mzero = return []

let ( >>= ) p f =
    let f' = fun (x, input') -> f x input' in
    fun input ->
        LazyList.concat (LazyList.map f' (p input))

let ( >> ) p q = p >>= fun _ -> q

let ( >>? ) f p q =
    p >>= fun x ->
    q >>= fun y ->
    return (f x y)

let ( >>:: ) p q =
    let cons = fun x xs -> x :: xs in
    ( >>? ) cons p q

let ( >>@ ) p q = ( >>? ) ( @ ) p q

(*
 * This expression:
 *
 *     (p >>:: (q >>:: r))
 *
 * Can be simplified to:
 *
 *     p ^>>:: q ^>>:: r
 *
 * This is why the next operators may result handy.
 *)

(* `^>>::' is right associative, whereas `>>::' is left associative *)
let ( ^>>:: ) = ( >>:: )

(* `^>>@' is right associative, whereas `>>@' is left associative *)
let ( ^>>@ ) = ( >>@ )

(* Non-deterministic alternative operator, tries all possibilities *)
let ( ||| ) p q = fun input -> LazyList.append (p input) (q input)

(* Deterministic alternative operator, tries `q' only if `p' fails *)
let ( <|> ) p q =
    fun input ->
        let res = p input in
        if LazyList.force res = LazyList.Nil
        then q input
        else res

(* Help operator, if `p' fails then raises an error with some embedded info *)
let ( <?> ) p info = p <|> fun input -> raise (Parser.Error (info, input))

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

let between o p c = drop o >> p >>@ drop c

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

let choice ps = List.fold_right ( ||| ) ps mzero

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

let many_till p until = many p >>@ drop until

let not_followed_by p =
    let pred_not p =
        scan >>= fun x ->
            if not (p x) then return x else fail
    in drop (pred_not p)

let rec count n p =
    match n with
    | n when n < 0 -> fail
    | 0            -> mzero
    | n            -> p >>:: count (n - 1) p

let pred p =
    item >>= fun x ->
        if p x then return x else fail

let any = pred (fun _ -> true)

let char c = pred (( = ) c)

let digit = pred (fun c -> '0' <= c && c <= '9')

let lower = pred (fun c -> 'a' <= c && c <= 'z')

let upper = pred (fun c -> 'A' <= c && c <= 'Z')

let letter = lower ||| upper

let alphanum = letter ||| digit

let word = many1 (alphanum ||| char '_')

let nat = many1 digit

let neg = char '-' >>:: nat

let int = neg ||| nat

let space = char ' ' ||| char '\t' ||| char '\r'

let eol = char '\n'

let sep = space ||| eol

let junk = drop (many sep)

let token p = p >>@ junk

let integer_of_token p =
    token p >>= fun x ->
    return (int_of_string (Backpack.Str.implode x))

let natural = integer_of_token nat

let negative = integer_of_token neg

let integer = negative ||| natural

let string s =
    let rec string' = function
        | []      -> mzero
        | c :: cs -> char c >>:: string' cs
    in string' (Backpack.Str.explode s)

let keyword s = token (string s)

let eof =
    fun input ->
        if Parser.empty input
        then mzero input
        else fail input

let junk_eof = junk >> eof

let junk_start p = junk >> p

let arith_op =
    (char '+' >> return ( + )) |||
    (char '-' >> return ( - )) |||
    (char '*' >> return ( * )) |||
    (char '/' >> return ( / ))
