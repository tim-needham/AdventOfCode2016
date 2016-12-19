module Day19

open System;
open System.IO

let rec maxpow (e : int) (p : int) (n : int) : int =
    match (float p)**(float e) with
        | m when m > (float n) -> e - 1;
        | _ -> maxpow (e+1) p n;
    

let josephus (n : int) : int =
    let a = maxpow 0 2 n;
    let l = n - int (2.0 ** float a);
    (2 * l) + 1;
    
// sisyphus because why not?
let sisyphus (n : int) : int =
    let a = maxpow 0 3 n;
    let l = n - int (3.0 ** float a);
    let m = int (3.0 ** float a);

    // when my brain starts working, I'll refactor out the if/then/else into a nice formula.
    if l = 0 then
        n;
    else if (l < m) then
        l;
    else
        m + (2 * (l - m));

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0] |> Int32.Parse;

    input
    |> josephus
    |> printfn "Day 19, part 1: %d";

    input
    |> sisyphus
    |> printfn "Day 19, part 2: %d";
