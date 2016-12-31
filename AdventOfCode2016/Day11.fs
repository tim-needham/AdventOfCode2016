module Day11

open System;
open System.Collections.Generic;
open System.IO;

type Component =
    | Chip of s : string
    | Gen of s : string;;

type Lab = {
    Lift : int;
    Floors : Component list list
};;

type Generic = {
    L : int;
    Fs : (int * int) list
};;

let rec parseStuff (s : string list) : Component list =
    match s with
        | [] -> [];
        | "a"::xs -> parseStuff xs;
        | "and"::xs -> parseStuff xs;
        | "nothing"::xs -> [];
        | x::"generator"::xs -> Gen x :: parseStuff xs;
        | x::"microchip"::xs -> Chip (x.Split('-').[0]) :: parseStuff xs;

let parseFloor (s : string) : Component list =
    match s.Split([|' '; ','; '.'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | "The"::n::"floor"::"contains"::xs -> parseStuff xs |> List.sort; 

let kind (c : Component) : string =
    match c with
        | Chip x -> x;
        | Gen x -> x;

let isChip (c : Component) : bool =
    match c with
        | Chip x -> true;
        | _ -> false;

let genericise (l : Lab) : Generic =
    {
        L = l.Lift
        Fs = l.Floors
                |> List.map (fun x -> x |> List.partition (isChip))
                |> List.map (fun (x, y) -> (x |> List.length, y |> List.length))
    };

let validFloor (cs : Component list) : bool =
    List.isEmpty cs
    ||
    List.forall (isChip) cs
    ||
    List.forall (isChip >> not) cs
    ||
    (
        let (hs, gs) = List.partition (isChip) cs;
        List.forall (fun h -> List.tryFindIndex (fun g -> kind g = kind h) gs <> None) hs;
    );

let validLab (l : Lab) : bool =
    l.Floors
    |> List.forall (validFloor);

let success (l : Lab) : bool =
    l.Floors
    |> List.take 3
    |> List.forall (List.isEmpty);

let rec singles (os : 'a list) (cs : 'a list) : ('a list * 'a list) list =
    match cs with
        | [] -> [];
        | x::xs -> ([x], os@xs) :: (singles (os@[x]) xs);

let doubles (cs : 'a list) : ('a list * 'a list) list =
    singles [] cs
    |> List.map (fun (x, y) -> (singles [] y) |> List.map (fun (p, q) -> (x@p, q)))
    |> List.collect id
    |> List.map (fun (x, y) -> (x |> List.sort, y |> List.sort))
    |> List.distinct
    |> List.sort;

let cargo (cs : Component list) : (Component list * Component list) list =
    doubles cs @ (singles [] cs) 
    |> List.filter (fun (x, y) -> validFloor y);

let newLab (c : Component list * Component list) (l : Lab) (d : int) : Lab =
    {
        Lift = l.Lift + d;
        Floors = [for i in 0..3 ->
                    if i = l.Lift + d then
                        l.Floors.[l.Lift + d] @ (fst c) |> List.sort;
                    else if i = l.Lift then
                        snd c;
                    else
                        l.Floors.[l.Lift];
                ]
    };

let moves (l : Lab) : Lab list =
    let cs = cargo l.Floors.[l.Lift];
    let l1, l2, l3, l4 = l.Floors.[0], l.Floors.[1], l.Floors.[2], l.Floors.[3];

    match l.Lift with
        | 0 ->  cs |> List.map (fun (x, y) -> { Lift = 1; Floors = [y; l2@x; l3; l4] });
        | 1 ->  (cs |> List.map (fun (x, y) -> { Lift = 2; Floors = [l1; y; l3@x; l4] }))
                @
                (cs |> List.map (fun (x, y) -> { Lift = 0; Floors = [l1@x; y; l3; l4] }));        
        | 2 ->  (cs |> List.map (fun (x, y) -> { Lift = 3; Floors = [l1; l2; y; l4@x] }))
                @
                (cs |> List.map (fun (x, y) -> { Lift = 1; Floors = [l1; l2@x; y; l4] }));
        | 3 -> cs
                |> List.map (fun (x, y) -> { Lift = 2; Floors = [l1; l2; l3@x; y] });

let validMoves (tr : Lab list * Dictionary<Generic, bool>) (l : Lab) : (Lab list * Dictionary<Generic, bool>) =
    let (ms, ss) = tr;

    let ns = moves l
            |> List.filter (validLab)
            |> List.map (fun x -> (x, genericise x))
            |> List.filter (snd >> ss.ContainsKey >> not)
            |> List.distinctBy (fun (x, y) -> y)
    
    for (x, y) in ns do
        ss.Add(y, true);

    ((ns |> List.map (fst))@ms, ss);

let rec bfs (m : int) (ls : Lab list) (ss : Dictionary<Generic, bool>) : int =

    let (ns, ts) = ls |> List.fold (validMoves) ([], ss);
    
    let os = ns |> List.filter (success);

    if os |> List.isEmpty then
        bfs (m+1) ns ts;
    else
        m+1;

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file));

    let floors = Seq.map (parseFloor) input
                |> Seq.toList;

    let lab =  { Lift = 0; Floors = floors };

    let part2 = [Chip "dilithium"; Chip "elerium"; Gen "dilithium"; Gen "elerium" ];

    let lab2 = 
        {
            Lift = 0;
            Floors = [for f in 0..3 -> lab.Floors.[f] @ if f = 0 then part2 else []]
        };

    let seen = new Dictionary<Generic, bool>();

    bfs 0 [lab] seen
    |> printfn "Day 11, part 1: %A";

    let seen2 = new Dictionary<Generic, bool>();

    bfs 0 [lab2] seen2
    |> printfn "Day 11, part 2: %d";