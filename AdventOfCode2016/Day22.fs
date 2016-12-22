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

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file))
                |> List.skip 2
                |> List.map (parse);

    validPairs input
    |> printfn "Day 22, part 1: %d";

    0
    |> printfn "Day 22, part 2: %d";