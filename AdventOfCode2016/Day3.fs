module Day3

open System;
open System.IO;

let triangle (t : int * int * int) : bool =
    let (a, b, c) = t;
    (a + b > c) && (a + c > b) && (b + c > a);

let parse (s : string) : (int * int * int) =
    let parts = s.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries);
    (Int32.Parse parts.[0], Int32.Parse parts.[1], Int32.Parse parts.[2]);

let rec zip3 (t : int list) (ts : (int * int * int) list) (i : int list) : (int * int * int) list =
    match t with
        | [] -> match i with
                | [] -> ts
                | is -> zip3 [(List.head is)] ts (List.tail is);
        | [x] -> zip3 (x :: [List.head i]) ts (List.tail i);
        | x::xs -> zip3 [] ((x, List.head xs, List.head i) :: ts) (List.tail i);

let rezip (t : (int * int * int) list) : (int * int * int) list =
    let (p, q, r) = Seq.fold (fun (a, b, c) (x, y, z)-> (a @ [x], b @ [y], c @ [z])) ([], [], []) t;
    let ts = p @ q @ r;
    zip3 [] [] ts;

let run (file : string) =
    let lines = Seq.toList (File.ReadLines(file));

    lines
    |> Seq.fold (fun a c -> a + if triangle (parse c) then 1 else 0) 0
    |> printfn "Day 3, part 1: %d";

    lines
    |> Seq.map (fun x -> parse x)
    |> Seq.toList
    |> rezip
    |> Seq.fold (fun a c -> a + if triangle c then 1 else 0) 0
    |> printfn "Day 3, part 2: %d";

