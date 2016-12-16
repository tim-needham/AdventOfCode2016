module Day16

open System;
open System.IO;

let dragon (i : string) : string =
    i + ("0"  + (i |> Seq.toList |> List.rev |> List.map (fun x -> if x = '1' then '0' else '1') |> String.Concat));

let rec pad (n : int) (i : string) : string =
    if i.Length >= n then
        i.Substring(0, n);
    else
        pad n (dragon i);

let rec check (i : string) : string =
    i.ToCharArray()
    |> Seq.pairwise
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (i, _) -> i % 2 = 0)
    |> Seq.map (fun (_, (x, y)) -> if x = y then '1' else '0')
    |> String.Concat;

let rec checkSum (i : string) : string =
    match i.Length with
        | n when n % 2 = 0 -> checkSum (check i);
        | _  -> i;

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0];
    
    pad 272 input 
    |> checkSum
    |> printfn "Day 16, part 1: %s";

    pad 35651584 input 
    |> checkSum
    |> printfn "Day 16, part 2: %s";