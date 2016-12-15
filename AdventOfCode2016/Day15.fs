module Day15

open System;
open System.IO;

let parse (s : string) : int * int =
    match s.Split([| ' '; '.' |], StringSplitOptions.RemoveEmptyEntries) with
        | [|_; _; _; x; _; _; _; _; _; _; _; y |] -> (Int32.Parse y, Int32.Parse x);

let pass (t : int) (s : int * int) : bool =
    let (i, p) = s;
    (i + t + 1) % p = 0;


let rec attempt (t: int) (s : (int * int) list) : bool =
    match s with
        | [] -> true;
        | x::xs ->  if pass t x then
                        attempt (t+1) xs;
                    else
                        false;

let rec drop (t : int) (s : (int * int) list) : int =
    match attempt t s with
        | true -> t;
        | false -> drop (t+1) s;

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file));
    
    let discs = input
                |> List.map (fun x -> parse x);

    drop 0 discs
    |> printfn "Day 15, part 1: %d";

    let part2 = "Disc #7 has 11 positions; at time=0, it is at position 0."

    let discs2 = discs @ [parse part2];
    
    drop 0 discs2
    |> printfn "Day 15, part 2: %d";