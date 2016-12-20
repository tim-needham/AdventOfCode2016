module Day19

open System;
open System.IO

let josephus (n : int) : int =
    let a = Math.Floor (Math.Log10 (float n) / Math.Log10 (2.0));
    let l = n - int (2.0 ** a);
    (2 * l) + 1;
    
// sisyphus because why not?
let sisyphus (n : int) : int =
    let a = Math.Floor (Math.Log10 (float n) / Math.Log10 (3.0));
    let c = int (3.0 ** a);
    let l = n - c;
    let b = int (Math.Floor (float l / float c));

    if l = 0 then
        n;
    else
        ((b + 1) * l) - (b * c);

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0] |> Int32.Parse;

    input
    |> josephus
    |> printfn "Day 19, part 1: %d";

    input
    |> sisyphus
    |> printfn "Day 19, part 2: %d";
