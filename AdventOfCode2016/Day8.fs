module Day8

open System;
open System.IO;

type Instruction =
    | Rect of x : int * y : int
    | Row of y : int * by : int
    | Column of x : int * by : int;;

let (|Pattern|_|) (i : string) (s : string)  =
    if s.StartsWith(i) then
        Some(s.Substring(i.Length));
    else
        None;

let parse (s : string) : Instruction =
    match s with
    | Pattern "rect " dims ->   let coords = dims.Split('x')
                                            |> Seq.map (fun x -> Int32.Parse x)
                                            |> Seq.toList;
                                Rect (coords.[0], coords.[1]);
    | Pattern "rotate row " rest ->     let parts = rest.Split(' ');
                                        let row = parts.[0].Split('=') |> Seq.toList |> List.rev |> List.head |> Int32.Parse;
                                        let by = parts |> Seq.toList |> List.rev |> List.head |> Int32.Parse;
                                        Row (row, by);
    | Pattern "rotate column " rest ->  let parts = rest.Split(' ');
                                        let col = parts.[0].Split('=') |> Seq.toList |> List.rev |> List.head |> Int32.Parse;
                                        let by = parts |> Seq.toList |> List.rev |> List.head |> Int32.Parse;
                                        Column (col, by);

let rec transpose (i : int list list) : int list list = 
    match i with 
        | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M);
        | _ -> [];

let apply (i : Instruction) (s : int list list) : int list list =
    match i with
        | Rect (x, y) ->    [for q in 1..6 -> [for p in 1..50 -> if p <= x && q <= y then 1 - s.[q-1].[p-1] else s.[q-1].[p-1]]];
        | Row (y, b) ->     [for q in 0..5 -> if q <> y then s.[q] else (s.[q] |> List.rev |> Seq.take b |> Seq.toList |> List.rev) @ (s.[q] |> Seq.take (50-b) |> Seq.toList)];
        | Column (x, b) ->  let t = transpose s;
                            transpose [for p in 0..49 -> if p <> x then t.[p] else (t.[p] |> List.rev |> Seq.take b |> Seq.toList |> List.rev) @ (t.[p] |> Seq.take (6-b) |> Seq.toList)];

let on (s : int list list) : int =
    s |> Seq.fold(fun a c -> a + (c |> Seq.fold (fun b d -> b + d) 0)) 0;

let rec pp (s : int list) (o : string) : string =
    match s with
        | [] -> o;
        | x::xs -> pp xs (o + if x = 1 then "#" else " ")

let run (file : string) =
    let instructions = Seq.toList (File.ReadLines(file))
                        |> Seq.map (fun x -> parse x) 
                        |> Seq.toList;

    let screen = instructions
                |> Seq.fold (fun a c -> apply c a) [for y in 1..6 -> [for x in 1..50 -> 0]];

    screen
    |> on
    |> printfn "Day 8, part 1: %d";

    printfn "Day 8, part 2:";
   
    let pretty = screen
                |> Seq.map (fun x -> pp x "");

    for r in pretty do 
        printfn "%A" r;
