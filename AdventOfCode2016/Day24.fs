module Day24

open System;
open System.Collections.Generic;
open System.IO;

type Square =
    | Empty
    | Wall
    | Target of t : int;;

type State =
    {
        X : int;
        Y : int
    };;

let parse (c : char) : Square =
    match c with
        | '#' -> Wall;
        | '.' -> Empty;
        | n ->  let x = Int32.Parse (string n);
                Target x;

let parseLine (s : string) : Square array =
    s.ToCharArray()
    |> Array.map (parse);

let ppS (s : Square) : char =
    match s with
        | Empty -> '.';
        | Wall -> '#';
        | Target t -> char (string t);

let pp (ss : Square array) : string =
    ss
    |> Array.map (ppS)
    |> String.Concat;

let moves (s : State) (m : Square array array) : State list =
    let bx = m.[0].Length;
    let by = m.Length;

    seq {
        if s.X > 0 then
            yield (s.X - 1, s.Y);
        if s.X < bx then
            yield (s.X + 1, s.Y);
        if s.Y > 0 then 
            yield (s.X, s.Y - 1);
        if s.Y < by then
            yield (s.X, s.Y + 1);    
    }
    |> Seq.filter (fun (x, y) -> m.[y].[x] <> Wall)
    |> Seq.map (fun (x, y) -> (x, y, m.[y].[x]))
    |> Seq.toList
    |> List.map (fun (x, y, z) -> { X = x; Y = y })

let validMoves (ms : Square array array) (tr : State list * Dictionary<State, bool>) (p : State) : (State list * Dictionary<State, bool>) =
    let (os, ss) = tr;

    let ns = moves p ms
            |> List.filter (fun x -> not (ss.ContainsKey(x)))
            |> List.distinct;
    
    for n in ns do
        ss.Add(n, true);

    (ns@os, ss);

let rec bfs (m : int) (t : int) (ms : Square array array) (ps : State list) (ss : Dictionary<State, bool>) : int = 
    let (qs, ts) = ps |> List.fold (fun a c -> validMoves ms a c) ([], ss);

    let rs = qs |> List.filter (fun a -> ms.[a.Y].[a.X] = Target t);

    if rs |> List.isEmpty then
        bfs (m+1) t ms qs ts;
    else
        m+1;

let rec findEntry (t : int) (x : int) (y : int) (ms : Square array array) : int * int =
    match ms.[y].[x] with
        | Target s when s = t -> (x, y);
        | _ ->  if x = ms.[0].Length - 1 then
                    findEntry t 0 (y + 1) ms;
                else
                    findEntry t (x + 1) y ms;

let shortestPath (a : int) (b : int) (ms : Square array array) : int =
    let x, y = findEntry a 0 0 ms;
    let entry = { X = x; Y = y };
    bfs 0 b ms [entry] (new Dictionary<State, bool>());

let rec route (c : int) (rs : int list) (g : (int * int * int) list) : int =
    match rs with
        | [] -> c;
        | [x] -> c;
        | x::y::xs ->   let d, e, f = g |> List.find (fun (p, q, r) -> x = p && y = q);
                        route (c+f) (y::xs) g;

let distrib (e : 'a) (ls : 'a list) : seq<'a list> =
    let rec aux pre post = 
        seq {
            match post with
                | [] -> yield (ls @ [e]);
                | x::xs ->  yield (List.rev pre @ [e] @ post);
                            yield! aux (x::pre) xs;
        }
    aux [] ls;

let rec perms (ls : 'a list) : seq<'a list> = 
    match ls with
        | [] -> Seq.singleton [];
        | x::xs -> Seq.collect (distrib x) (perms xs);
    
let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> Seq.map (parseLine)
                |> Seq.toArray;

    let edges = [for i in [0..7] -> [for j in [(i+1)..7] -> (i, j)]]
                |> List.collect id
                |> List.map (fun (a, b) -> (a, b, shortestPath a b input));

    let graph = (edges @ (edges |> List.map (fun (a, b, c) -> (b, a, c))))
                |> List.sortBy (fun (a, b, c) -> a, b);

    [1..7]
    |> perms
    |> Seq.toList
    |> List.map (fun x -> route 0 (0::x) graph)
    |> List.min
    |> printfn "Day 24, part 1: %d";

    [1..7]
    |> perms
    |> Seq.toList
    |> List.map (fun x -> route 0 (0::(x@[0])) graph)
    |> List.min
    |> printfn "Day 24, part 2: %d";