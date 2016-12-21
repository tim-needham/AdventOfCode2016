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

let rec unscramble (is : Instruction list) (s : string) (ps : string list) : string =
    match ps with
        | [] -> "";
        | x::xs ->  let y = x |> Seq.toList;
                    let scrambled = is
                                    |> Seq.toList
                                    |> List.fold (fun a c -> apply c a) y
                                    |> String.Concat;

                    if scrambled = s then
                        x;
                    else
                        unscramble is s xs;

let rec distribute (e : 'a) (es : 'a list) : 'a list list =
    match es with
        | [] -> [[e]];
        | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs];

let rec permute (cs : 'a list) : 'a list list =
    match cs with
        | [] -> [[]];
        | e::xs -> List.collect (distribute e) (permute xs);

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> List.map (parse);

    let password = "abcdefgh"
                    |> Seq.toList;

    input
    |> List.fold (fun a c -> apply c a) password;
    |> String.Concat
    |> printfn "Day 21, part 1: %s";

    // It would be nice to think that we could just reverse the instruction list
    // and apply the inverse of each operation but RotateP is surjective meaning
    // that its inverse is not well-defined. In other words we'd end up with a 
    // tree full of potentially valid solutions and that point it's easier to 
    // note that there are no instructions that alter the input length and
    // some instructions rely on there only being one copy of each character
    // so it's easier to just permute the input sequence and brute force a 
    // solution.

    let scrambled = "fbgdceah";

    "abcdefgh"
    |> Seq.toList
    |> permute
    |> List.map (String.Concat)
    |> unscramble input scrambled
    |> printfn "Day 21, part 2: %s";