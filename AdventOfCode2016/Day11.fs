module Day11

open System;
open System.IO;

type Component =
    | Chip of s : string
    | Gen of s : string;;

type Floor = Component list;;

type Lab = 
    {
        Lift : int;
        Floors : Floor list
    };;

let isChip (c : Component) : bool =
    match c with
        | Chip _ -> true;
        | Gen _ -> false;

let isGen (c : Component) : bool =
    match c with
        | Gen _ -> true;
        | Chip _ -> false;

let rec parseStuff (s : string list) : Component list =
    match s with
        | [] -> [];
        | "a"::xs -> parseStuff xs;
        | "and"::xs -> parseStuff xs;
        | "nothing"::xs -> [];
        | x::"generator"::xs -> Gen x :: parseStuff xs;
        | x::"microchip"::xs -> Chip (x.Split('-').[0]) :: parseStuff xs;

let parseFloor (s : string) : Floor =
    match s.Split([|' '; ','; '.'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | "The"::n::"floor"::"contains"::xs -> parseStuff xs |> List.sort; 

let success (l : Lab) : bool =
    l.Floors
    |> List.take 3
    |> List.forall (List.isEmpty);

let rec validFloor (f : Floor) : bool =
    match f with
        | [] -> true;
        | Gen x::xs -> true;
        | Chip x::xs -> (xs |> List.isEmpty
                        ||
                        xs |> List.forall (isChip)
                        ||
                        xs |> List.tryFindIndex (fun y -> y = Gen x) <> None)
                        &&
                        validFloor xs;                        

let validLab (l : Lab) : bool =
    l.Floors
    |> List.forall (validFloor);

let pick (f : Floor) : Component list list =
    let d = if f |> List.length < 2 then
                [];
            else
                [for x in f do for y in f -> (x, y)]
                |> List.filter (fun (x, y) -> x <> y)
                |> List.map (fun (x, y) -> [x; y] |> List.sort)
                |> List.distinct;

    let s = if f |> List.isEmpty then
                [];
            else
                [for x in f -> [x]];
    d@s;

let newLab (l : Lab) (cs : Component list) (d : int) : Lab =
    {
        Lift = l.Lift + d;
        Floors =    [
            for f in 0..3 ->
                if f = l.Lift+d then
                    l.Floors.[f] @ cs |> List.sort;
                else if f = l.Lift then
                    (Set.ofList l.Floors.[f]) - (Set.ofList cs)
                    |> Set.toList
                    |> List.sort;
                else
                    l.Floors.[f];
        ]
    }

let moves (ts : Lab list) (ss : Lab list): Lab list list=
    let h, t = ts |> List.head, ts |> List.tail;
    let ms = pick h.Floors.[h.Lift];

    let ds =    if h.Lift > 0  && not (h.Floors.[h.Lift - 1] |> List.isEmpty) then
                    ms
                    |> List.map (fun x -> newLab h x -1);
                else
                    [];

    let us =    if h.Lift < 3 then
                    ms
                    |> List.map (fun x -> newLab h x 1);
                else
                    [];

    let ms = (us @ ds)    
            |> List.filter (validLab)
            |> Set.ofList;

    (ms - Set.ofList (t)) - Set.ofList(ss)
    |> Set.toList
    |> List.map (fun x -> x::ts);

let cost (l : Lab) : int =
    l.Floors
    |> List.rev
    |> List.zip [0..3]
    |> List.map (fun (x, y) -> x * (y |> List.length))
    |> List.sum;

let rec search (ss : Lab list) (q : Lab list list) : Lab list =
    match q with
        | [] -> [];
        | x::xs ->  let h = x |> List.head;
                    if success h then
                        x;
                    else
                        let ms = moves x ss;
                        let ns = (xs@ms)
                                |> List.sortBy (List.head >> cost);
                        search (h::ss) ns;

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file));

    let floors = Seq.map (parseFloor) input
                |> Seq.toList;

    let lab =  { Lift = 0; Floors = floors };

    search [] [[lab]]
    |> List.tail
    |> List.length
    |> printfn "Day 11, part 1: %d";

    let part2 = [Chip "dilithium"; Chip "elerium"; Gen "dilithium"; Gen "elerium"];

    let lab2 = 
        {
            Lift = 0;
            Floors = 
                [
                    for f in 0..3 ->
                        if f = 0 then
                            (floors.[f] @ part2) |> List.sort;
                        else
                            floors.[f]
                ];
        };

    search [] [[lab2]]
    |> List.tail
    |> List.length
    |> printfn "Day 11, part 2: %d";




