(*
 * My personal OCaml backpack
 *
 * I never leave home without it
 *)

module type MONAD =
    sig
        type 'a t
        val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
        val ( >> )  : 'a t -> 'b t -> 'b t
        val return  : 'a -> 'a t
        val fail    : string -> 'a t
    end

module OptionMonad : MONAD =
    struct
        type 'a t = 'a option

        let ( >>= ) o f =
            match o with
            | Some x -> f x
            | None   -> None

        let ( >> ) o o' = o >>= fun _ -> o'

        let return x = Some x

        let fail = failwith
    end

module StringMap = Map.Make (String)
module IntMap    = Map.Make (struct type t = int let compare = compare end)

(* Function composition operator *)
let ( |. ) f g = fun x -> f (g x)

(* Reduce function application precedence operator *)
let ( $ ) f x = f x

(* Reverse function application operator *)
let ( |> ) x f = f x

let id x = x

let explode s =
    let rec explode' i l =
        if i < 0 then l else explode' (i - 1) (s.[i] :: l) in
    explode' (String.length s - 1) []

let implode l =
    let res = String.create (List.length l) in
    let rec implode' i = function
        | [] -> res
        | c :: l -> res.[i] <- c; implode' (i + 1) l in
    implode' 0 l
