(*
 * My personal OCaml backpack
 *
 * I never leave home without it
 *)

module StringMap = Map.Make (String)

module IntMap = Map.Make (struct type t = int let compare = compare end)

module OptionMonad =
    struct
        let ( >>= ) o f =
            match o with
            | Some x -> f x
            | None   -> None

        let ( >> ) o o' = o >>= fun _ -> o'

        let return x = Some x

        let fail = failwith
    end

module Op =
    struct
        let id x = x

        (* Function composition operator *)
        let ( |. ) f g = fun x -> f (g x)

        (* Reduce function application precedence operator *)
        let ( $ ) f x = f x

        (* Reverse function application operator *)
        let ( |> ) x f = f x
    end

module Str =
    struct
        let explode s =
            let rec explode' i l =
                if i < 0 then l else explode' (i - 1) (s.[i] :: l)
            in explode' (String.length s - 1) []

        let implode l =
            let res = String.create (List.length l) in
            let rec implode' i = function
                | [] -> res
                | c :: l -> res.[i] <- c; implode' (i + 1) l
            in implode' 0 l
    end

module InfiniteList =
    struct
        type 'a infinite_list =
            | Nil
            | Cons of 'a * (unit -> 'a infinite_list)

        let head = function
            | Nil         -> None
            | Cons (x, _) -> Some x

        let rec tail = function
            | Nil         -> None
            | Cons (_, f) -> Some (f ())

        let rec range n0 inc = Cons (n0, fun () -> range (n0 + inc) inc)

        let range0 inc = range 0 inc

        let range_naturals = range0 1
    end
