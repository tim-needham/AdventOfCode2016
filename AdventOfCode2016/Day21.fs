module Day21

open System;
open System.IO;

type Direction =
    | L
    | R;;

type Instruction =
    | SwapP of x : int * y : int
    | SwapL of x : char * y : char
    | Rotate of d : Direction * x : int
    | RotateP of x : char
    | Reverse of x : int * y : int
    | Move of x : int * y : int;;

let parse (s : string) : Instruction =
    match s.Split(' ') |> Array.toList with
        | "swap"::"position"::x::_::_::y::_ ->  let a, b = Int32.Parse x, Int32.Parse y;
                                                SwapP (min a b, max a b);
        | "swap"::"letter"::x::_::_::y::_ ->    SwapL (char x, char y);
        | "rotate"::"left"::x::_ -> Rotate (L, Int32.Parse x);
        | "rotate"::"right"::x::_ -> Rotate (R, Int32.Parse x);
        | "rotate"::"based"::_::_::_::_::x::_ -> RotateP (char x);
        | "reverse"::"positions"::x::_::y::_ -> let a, b = Int32.Parse x, Int32.Parse y;
                                                Reverse (min a b, max a b);
        | "move"::_::x::_::_::y::_ -> Move (Int32.Parse x, Int32.Parse y);

// x < y
let swapP (x : int) (y : int) (cs : char list) : char list =
    let is = match x with
                | 0 -> [];
                | _ -> cs |> List.take x;

    let js = cs
            |> List.skip (x + 1)
            |> List.take (y - x - 1);

    let ks = match y with
                | n when n = (cs |> List.length) - 1 -> [];
                | _ -> cs |> List.skip (y + 1);

    is @ [(cs |> List.item y)] @ js @ [(cs |> List.item x)] @ ks;

let rec swapL (x : char) (y : char) (cs : char list) : char list =
    match cs with
        | [] -> [];
        | d::ds when d = x ->  y :: swapL x y ds;
        | d::ds when d = y ->  x :: swapL x y ds;
        | d::ds ->  d :: swapL x y ds;

let rotate (d : Direction) (x : int) (cs : char list) : char list =
    let n = cs |> List.length
    let y = x % n;
    match d with
        | L ->  (cs |> List.skip y) @ (cs |> List.take y);
        | R ->  (cs |> List.skip (n - y)) @ (cs |> List.take (n - y));

let rotateP (x : char) (cs : char list) : char list =
    let y = cs |> List.findIndex (fun a -> a = x);

    match y with
        | n when n >= 4 -> rotate R (y + 2) cs;
        | _ -> rotate R (y + 1) cs;

let unrotateP (x : char) (cs : char list) : char list =
    let y = cs |> List.findIndex (fun a -> a = x);

    match y with
        | n when n % 2 = 1 -> rotate L ((y+1) / 2) cs;
        | 0 -> rotate L 1 cs; // y = 0 is actually y = 8 mod 8, then it fits the below.
        | _ -> rotate L ((y/2) + 5) cs;

// x < y
let reverse (x : int) (y : int) (cs : char list) : char list =
    let is = match x with
                | 0 -> [];
                | _ -> cs |> List.take x;

    let js = cs
            |> List.skip x
            |> List.take (y - x + 1)
            |> List.rev;

    let ks = match y with
                | n when n = (cs |> List.length) - 1 -> [];
                | _ -> cs |> List.skip (y + 1);

    is @ js @ ks;

let move (x : int) (y : int) (cs : char list) : char list =
    let is = match x with
                | 0 -> cs |> List.skip 1;
                | n when n = (cs |> List.length) -> cs |> List.take (x - 1);
                | _ -> (cs |> List.take x) @ (cs |> List.skip (x + 1));

    let i = cs |> List.item x;

    match y with
        | 0 -> i::is;
        | _ -> (is |> List.take y) @ [i] @ (is |> List.skip y);

let apply (i : Instruction) (cs : char list) : char list =
    match i with
        | SwapP (x, y) -> swapP x y cs;
        | SwapL (x, y) -> swapL x y cs;
        | Rotate (d, x) -> rotate d x cs;
        | RotateP x -> rotateP x cs;
        | Reverse (x, y) -> reverse x y cs;
        | Move (x, y) -> move x y cs;

let unapply (i : Instruction) (cs : char list) : char list =
    match i with
        | SwapP (x, y) -> swapP x y cs;
        | SwapL (x, y) -> swapL x y cs;
        | Rotate (L, x) -> rotate R x cs;
        | Rotate (R, x) -> rotate L x cs;
        | RotateP x -> unrotateP x cs;
        | Reverse (x, y) -> reverse x y cs;
        | Move (x, y) -> move y x cs;
    
let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> List.map (parse);

    let password = "abcdefgh"
                    |> Seq.toList;

    input
    |> List.fold (fun a c -> apply c a) password
    |> String.Concat
    |> printfn "Day 21, part 1: %s";

    let scrambled = "fbgdceah"
                    |> Seq.toList;

    input
    |> List.rev
    |> List.fold (fun a c -> unapply c a) scrambled
    |> String.Concat
    |> printfn "Day 21, part 2: %s";