module Day13

open System;
open System.IO;

let rec bin (i : int) : int list =
    match i with
        | 0 | 1 -> [i];
        | _ -> bin (i / 2) @ [(i % 2)];

let valid (x : int) (y : int) (k : int) : bool =
    let v = (x*x) + (3*x) + (2*x*y) + y + (y*y) + k
            |> bin 
            |> List.filter (fun a -> a = 1)
            |> List.length;
    v % 2 = 0;

let moves (p : int * int) (k : int) : (int * int) list =
    let (x, y) = p;
    [(x-1, y); (x, y-1); (x+1, y); (x, y+1)]
    |> Seq.filter (fun (a, b) -> a >= 0 && b >= 0)
    |> Seq.filter (fun (a, b) -> valid a b k)
    |> Seq.toList;

let shortest (p : int * int) (n : int) (ps : int list list) : int list list =
    let (a, b) = p;
    let maxY = List.length ps - 1;
    let maxX = List.length ps.[0] - 1;

    [for y in 0..maxY -> 
        [for x in 0..maxX ->
            if x = a && y = b then
                if ps.[y].[x] = -1 then
                    n;
                else if ps.[y].[x] > n then
                    n;
                else
                    ps.[y].[x];
            else
                ps.[y].[x];
        ]
    ];

let rec walk (ps : int list list) (p : int * int) (k : int) (m : (int * (int * int)) list) : int =
    match m with
        | [] -> 0;
        | (n, x)::xs -> if x = p then
                            n
                        else
                            let rs = shortest x n ps;
                            let ms = moves x k
                                    |> List.filter (fun (a, b) -> rs.[b].[a] = -1)
                                    |> List.map (fun y -> (n+1, y));
                            walk rs p k (xs @ ms);

let rec roam (ps : int list list) (l : int) (k : int) (m : (int * (int * int)) list) : int list list =
    match m with
        | [] -> [];
        | (n, (x, y))::xs ->    if n > l then
                                    ps;
                                else
                                    let rs = shortest (x, y) n ps;
                                    let ms = moves (x, y) k
                                            |> List.filter (fun (a, b) -> rs.[b].[a] = -1 || rs.[b].[a] > n + 1)
                                            |> List.map (fun z -> (n+1, z));
                                    roam rs l k (xs @ ms);
                                


let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0]
                |> Int32.Parse;

    walk [for y in 0..100 -> [for x in 0..100 -> -1]] (31, 39) input [(0, (1, 1))]
    |> printfn "Day 13, part 1: %d";

    roam [for y in 0..100 -> [for x in 0..100 -> -1]] 50 input [(0, (1, 1))]
    |> List.fold (fun a c -> a + List.fold (fun b d -> b + if d >=0 then 1 else 0) 0 c) 0
    |> printfn "Day 13, part 2: %d";