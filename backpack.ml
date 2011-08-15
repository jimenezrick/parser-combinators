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
        type 'a t =
            | Nil
            | Cons of 'a * (unit -> 'a t)

        let head = function
            | Nil         -> None
            | Cons (x, _) -> Some x

        let tail = function
            | Nil         -> None
            | Cons (_, f) -> Some (f ())

        let rec range n0 inc = Cons (n0, fun () -> range (n0 + inc) inc)

        let range0 inc = range 0 inc

        let range_naturals = range0 1
    end

module LazyList =
    struct
        type 'a node =
            | Nil
            | Cons of 'a * 'a t
        and 'a t = 'a node Lazy.t

        let from f =
            let rec next n =
                match f n with
                | None   -> Nil
                | Some x -> Cons (x, lazy (next (n + 1)))
            in lazy (next 0)

        let of_list l =
            let rec next = function
                | []      -> Nil
                | x :: xs -> Cons (x, lazy (next xs))
            in lazy (next l)

        let of_string s =
            let rec next n =
                try Cons (String.get s n, lazy (next (n + 1))) with
                | Invalid_argument _ -> Nil
            in lazy (next 0)

        let of_stream s =
            let rec next () =
                try Cons (Stream.next s, lazy (next ())) with
                | Stream.Failure -> Nil
            in lazy (next ())

        let of_channel c = of_stream (Stream.of_channel c)

        let force l =
            match Lazy.force l with
            | Nil         -> None
            | Cons (h, t) -> Some (h, t)

        let map f l =
            let rec next f l =
                match Lazy.force l with
                | Nil         -> Nil
                | Cons (h, t) -> Cons (f h, lazy (next f t))
            in lazy (next f l)

        let append l1 l2 =
            let rec next l1 l2 =
                match Lazy.force l1 with
                | Nil         -> l2
                | Cons (h, t) -> Cons (h, lazy (next t l2))
            in lazy (next l1 l2)
    end
