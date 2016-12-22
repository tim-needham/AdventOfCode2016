module Day22

open System;
open System.IO;

type Node = {
    Name : string;
    X : int;
    Y : int;
    Size : int;
    Used : int;
    Avail : int;
    Perc : int
};;

let parse (s : string) : Node = 
    match s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | a::b::c::d::e::_ ->   let p = Int32.Parse (b.Substring(0, b.Length - 1));
                                let q = Int32.Parse (c.Substring(0, c.Length - 1));
                                let r = Int32.Parse (d.Substring(0, d.Length - 1));
                                let s = Int32.Parse (e.Substring(0, e.Length - 1));
                                let l = a.Split('-');
                                let x = Int32.Parse (l.[1].Substring(1));
                                let y = Int32.Parse (l.[2].Substring(1));
                                {
                                    Name = a;
                                    X = x;
                                    Y = y;
                                    Size = p;
                                    Used = q;
                                    Avail = r;
                                    Perc = s;
                                }

let validPairs (ns : Node list) : int =
    [for i in ns -> [for j in ns -> (i, j)]]
    |> List.collect id
    |> List.filter (fun (x, _) -> x.Used > 0)
    |> List.filter (fun (x, y) -> x.Name <> y.Name)
    |> List.filter (fun (x, y) -> x.Used <= y.Avail)
    |> List.length;

let moves (p : int * int) (b : int * int) : (int * int) list =
    let px, py = p;
    let bx, by = b;

    seq {
        if px > 0 then
            yield (px - 1, py);
        if px < bx then
            yield (px + 1, py);
        if py > 0 then 
            yield (px, py - 1);
        if py < by then
            yield (px, py + 1);    
    }
    |> Seq.toList;

let find (x : int) (y : int) (ns : Node list) : Node =
    ns
    |> List.filter (fun a -> a.X = x && a.Y = y)
    |> List.head;

// Some nodes are significantly larger than others, these nodes effectively produce
// a wall that blocks passage. All other adjacent nodes are convienently arranged
// such that moving the data from one to another will be valid if the receiving
// node is empty.
let valid (p : int * int) (ns : Node list) : bool =
    let x, y = p;

    let n = ns |> find x y;

    n.Size < 100;

let validMoves (b : int * int) (ns : Node list) (tr : (int * int) list * (int * int) list) (p : int * int) : ((int * int) list * (int * int) list) =
    let (ms, ss) = tr;

    let ns = moves p b
            |> List.filter (fun x -> valid x ns)
            |> List.filter (fun x -> ss |> List.tryFindIndex (fun y -> y = x) = None)
            |> List.distinct;
    
    (ns@ms, ns@ss);

let rec bfs (m : int) (b : int * int) (ns : Node list) (t : int * int) (ps : (int * int) list) (ss : (int * int) list) : int = 
    let (qs, ts) = ps |> List.fold (fun a c -> validMoves b ns a c) ([], ss);

    let rs = qs |> List.filter (fun x -> x = t);

    if rs |> List.isEmpty then
        bfs (m+1) b ns t qs ts;
    else
        m+1;

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> List.skip 2
                |> List.map (parse);

    validPairs input
    |> printfn "Day 22, part 1: %d";

    let width, height = input |> List.map (fun a -> (a.X, a.Y)) |> List.maxBy (fun (a, b) -> a, b);
    let empty = input |> List.filter (fun a -> a.Used = 0) |> List.map (fun a -> (a.X, a.Y)) |> List.head;
    let goal = (width, 0);
    let target = (width - 1, 0);
    let entry = (0, 0);

    // First move the empty space to the left of the Goal.
    let m1 = bfs 0 (width, height) input target [empty] [];

    // Then move the Goal data to the Entry. We effectively do this by
    // swapping the blank with the Goal and then repeatedly repositioning
    // the empty space to the left of the Goal. Each time the empty space 
    // must be moved to the left of the Goal takes 5 moves. 
    //
    // 0    1    2    3    4    5
    // ._G  .G-  .G.  .G.  .G.  _G.
    // ...  ...  .._  ._.  _..  ...
    //
    // This then leaves us with the first 10 nodes on the first row as 
    // "_G........" and so we need one more move.
    m1 + ((width - 1) * 5) + 1
    |> printfn "Day 22, part 2: %d";