module Day6

open System;
open System.IO;

let rec transpose (c : char list list) : char list list = 
    match c with 
        | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M);
        | _ -> [];

let rec accumulate (s : (char * int) list) (c : char) : (char * int) list =
    match s with
        | [] -> [(c, 1)];
        | (x, y)::xs -> if x = c then
                            [(x, y+1)] @ xs;
                        else
                            [(x, y)] @ accumulate xs c;

let rec accrete (s : (char * int) list) (c : char list) : (char * int) list =
    match c with
        | [] -> s;
        | x::xs ->  accrete (accumulate s x) xs;

let run (file : string) =
    let lines = Seq.toList (File.ReadLines(file));

    let accreted = lines
                    |> Seq.map (fun x -> [for y in x -> y])
                    |> Seq.toList
                    |> transpose
                    |> Seq.map (fun x -> accrete [] x);

    accreted
    |> Seq.map (fun x -> x |> Seq.sortBy (fun (a, b) -> -b))
    |> Seq.map (fun x -> x |> Seq.head)
    |> Seq.map (fun (a, b) -> a)
    |> String.Concat
    |> printfn "Day 6, part 1: %A";

    accreted
    |> Seq.map (fun x -> x |> Seq.sortBy (fun (a, b) -> b))
    |> Seq.map (fun x -> x |> Seq.head)
    |> Seq.map (fun (a, b) -> a)
    |> String.Concat
    |> printfn "Day 6, part 2: %A";

