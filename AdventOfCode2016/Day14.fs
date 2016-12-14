module Day14

open Array.Parallel;
open System;
open System.IO;
open System.Security.Cryptography;
open System.Text;

[<StructuredFormatDisplay("{AsString}")>]
type Key = { Index : int; Hash : string }
            override m.ToString() = String.Format("Index {0} Hash {1}", m.Index, m.Hash)
            member m.AsString = m.ToString();;

[<StructuredFormatDisplay("{AsString}")>]
type Candidate = { Token : char; Value : Key }
                    override m.ToString() = String.Format("Token {0} {1}", m.Token, m.Value)
                    member m.AsString = m.ToString();;

let md5 (s : string) : string =
    use md5 = MD5.Create();
    s
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun x -> String.Format("{0:x2}", x))
    |> String.Concat;

let nplet (s : string) (n : int) : char option = 
    s
    |> Seq.windowed n
    |> Seq.tryFind (fun t -> [0..n-1] |> List.fold (fun a c -> a && t.[0] = t.[c]) true)
    |> Option.map (fun u -> u.[0]);

let rec hash (s : string) (n : int) : string =
    match n with
        | 0 -> md5 s;
        | _ -> hash (md5 s) (n-1);

let rec find (cs : Candidate list) (ks : Key list) (s : string) (i : int) (k : int) (n : int) : Key list =
    match List.length ks with
        | l when l >= k ->  ks
                            |> Seq.take k
                            |> Seq.toList
                            |> List.rev;
        | _ ->  let ts = [|i..i+999|]
                        |> Array.Parallel.map (fun x -> { Index = x; Hash = hash (s + x.ToString()) n })
                        |> Array.Parallel.map (fun y -> (nplet y.Hash 3, y))
                        |> Array.filter (fun (x, y) -> x <> None)
                        |> Array.map (fun (Some x, y) -> { Token = x; Value = y})
                        |> Array.toList;

                let fs = cs@ts
                        |> List.map (fun x -> (nplet x.Value.Hash 5, x.Value.Index))
                        |> List.filter (fun (x, y) -> x <> None)
                        |> List.map (fun (Some x, y) -> (x, y))
                        |> List.sort;

                let os = cs
                        |> List.filter (fun x -> fs |> List.tryFindIndex (fun (y, z) -> y = x.Token && (z - x.Value.Index) > 0 && (z - x.Value.Index) <= 1000) <> None)
                        |> List.map (fun x -> x.Value);

                find ts (ks@os) s (i+1000) k n;
   

let run (file : string) =
    let salt = (Seq.toList (File.ReadLines(file))).[0];

    find [] [] salt 0 64 0
    |> List.map (fun x -> x.Index)
    |> List.head
    |> printfn "Day 14, part 1: %d";

    find [] [] salt 0 64 2016
    |> List.map (fun x -> x.Index)
    |> List.head
    |> printfn "Day 14, part 2: %d";