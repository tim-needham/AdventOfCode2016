module Day18

open System;
open System.IO

let trap (x : char) (y : char) (z : char) : char =
    match x, y, z with
        | '^', '^', '.' -> '^';
        | '.', '^', '^' -> '^';
        | '^', '.', '.' -> '^';
        | '.', '.', '^' -> '^';
        | _, _, _ -> '.';

let generate (s : string) : string =
    ("."+s+".").ToCharArray()
    |> Array.toList
    |> Seq.windowed 3
    |> Seq.map (Array.toList >> (fun (x::y::z::_) -> trap x y z))
    |> String.Concat;

let rec safe (s : string) : int =
    s.ToCharArray()
    |> Array.toList
    |> List.filter (fun x -> x = '.')
    |> List.length;

let rec build (n : int) (i : int) (t : int) (r : string) : int =
    if i = n then
        t + safe r;
    else
        let s = generate r;
        let u = safe r;

        build n (i+1) (t+u) s;

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0];

    input
    |> build 40 1 0
    |> printfn "Day 18, part 1: %d";

    input
    |> build 400000 1 0
    |> printfn "Day 18, part 2: %d";