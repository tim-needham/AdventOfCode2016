module Day20

open System;
open System.IO;

let parse (s : string) : uint32 * uint32 =
    match s.Split('-') with
        | [|x; y|] -> (x |> UInt32.Parse, y |> UInt32.Parse);

let eliminate (r : uint32 * uint32) (b : uint32 * uint32) : (uint32 * uint32) list =
    if snd r < fst b then
        [r];
    else if fst r > snd b then
        [r];
    else if fst r >= fst b && snd r <= snd b then
        [];
    else
        let l = if fst r < fst b then
                    [(fst r, fst b - 1u)];
                else 
                    [];
        let u = if snd r > snd b then
                    [(snd b + 1u, snd r)];
                else
                    [];
        l@u;

let rec subtract (rs : (uint32 * uint32) list) (bs : (uint32 * uint32) list) : (uint32 * uint32) list =
    match bs with
        | [] -> rs;
        | x::xs ->  let ss = rs
                            |> List.map (fun r -> eliminate r x)
                            |> List.collect id;

                    subtract ss xs;

let rec range (rs : (uint32 * uint32) list) : uint32 =
    match rs with
        | [] -> 0u;
        | x::xs -> (snd x - fst x + 1u) + range xs;

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> List.map (parse)
                |> List.sortBy (fst);

    let whiteList = subtract [(0u, UInt32.MaxValue)] input;

    whiteList
    |> List.head
    |> fst
    |> printfn "Day 20, part 1: %d";

    whiteList
    |> range
    |> printfn "Day 20, part 2: %d";