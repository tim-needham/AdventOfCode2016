module Day1

open System;
open System.IO;

let (|Direction|_|) (d : string) (s : string) =
    if s.StartsWith(d) then
        Some(s.Substring(d.Length))
    else
        None

let modulo (x : int) (m : int) : int =
    let r = x % m;
    if r < 0 then r + m else r;

let turn (r : int) (d : int * int) : (int * int) =
    let compass = [| (0, 1); (1, 0); (0, -1); (-1, 0) |];
    let pos = Array.findIndex (fun (x, y) -> (x, y) = d) compass;
    compass.[modulo (pos + r) 4];

let walk (d : int * int) (p : (int * int) list) : (int * int) list =
    match p with
        | [] -> [d]
        | x::xs ->  let d' = (fst d + fst x, snd d + snd x);
                    d' :: p;

let rec compile (d : int * int) (l : int) (p : (int * int) list) : (int * int) list =
    match l with
        | 0 -> p;
        | _ -> compile d (l-1) (walk d p);

let ramble (m : string) (d : int * int) (p : (int * int) list) : (int * int) * (int * int) list =
    let r, l = match m with
                        | Direction "R" rest -> 1, Int32.Parse rest;
                        | Direction "L" rest -> -1, Int32.Parse rest;
                        | _ -> 0, 0;

    let d' = turn r d;
    (d', compile d' l p);

let rec revisit (p : (int * int) list) (r : (int * int) list) : int * int =
    match p, r with
        | [], x::xs -> revisit [x] xs
        | _, (x1, x2)::xs ->    match Seq.tryFindIndex (fun (y1, y2) -> x1 = y1 && x2 = y2) p with
                                    | Some i -> (x1, x2);
                                    | None -> revisit ((x1, x2) :: p) xs;

let distance (a : int * int) : int =
    let (x, y) = a;
    abs x + abs y;

let run (file : string) =
    let moves = (Seq.toList (File.ReadLines(file))).[0].Split ([|','; ' '|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.toList;

    let (direction, positions) = Seq.fold (fun a c -> ramble c (fst a) (snd a)) ((0, 1), [(0, 0)]) moves;

    positions
    |> Seq.head
    |> distance
    |> printfn "Day 1, part 1: %d";

    positions
    |> List.rev
    |> revisit []
    |> distance
    |> printfn "Day 1, part 2: %d";