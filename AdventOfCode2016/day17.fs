module Day17

open System;
open System.IO;
open System.Security.Cryptography;
open System.Text;

type Move = 
    | U
    | D
    | L
    | R;;

let pp (m : Move) : string = 
    match m with 
        | U -> "U";
        | D -> "D";
        | L -> "L";
        | R -> "R";

let md5 (s : string) : string =
    use md5 = MD5.Create();
    s
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun x -> String.Format("{0:x2}", x))
    |> String.Concat;

let rec walk (p : int * int) (ms : Move list) : int * int =
    match ms with
        | [] -> p;
        | x::xs ->  let (a, b) = p;
                    match x with
                        | U -> walk (a, b - 1) xs;
                        | D -> walk (a, b + 1) xs;
                        | L -> walk (a - 1, b) xs;
                        | R -> walk (a + 1, b) xs;

let rec trail (ms : Move list) : string =
    ms
    |> List.rev
    |> List.map (pp)
    |> String.Concat;

let isOpen (c : char) : bool =
    match c with
        | 'b' | 'c' | 'd' | 'e' | 'f' -> true;
        | _ -> false;

let next (h : string) : Move list =
    h
    |> Seq.take 4
    |> Seq.toList
    |> List.map (isOpen)
    |> List.zip [U; D; L; R]
    |> List.filter (fun (a, b) -> b)
    |> List.map (fun (a, b) -> a);

let valid (p : int * int) (m : Move) : bool =
    let (x, y) = p;
    match m with
        | U -> y > 0;
        | D -> y < 3;
        | L -> x > 0;
        | R -> x < 3;

let moves (k : string) (ms : Move list) (p : int * int) : Move list list =
    ms
    |> trail
    |> (fun x -> k + x)
    |> md5
    |> next
    |> List.filter (fun x -> valid p x)
    |> List.map (fun x -> x::ms);

let rec search (k : string) (ms : Move list list) : string =
    match ms with
        | [] -> "";
        | x::xs ->  let p = walk (0, 0) x;
                    if p = (3, 3) then
                        trail x;
                    else
                        let ns = moves k x p;
                        search k (xs @ ns);

let rec dfs (k : string) (o : int) (ms : Move list list) : int =
    match ms with
        | [] -> o;
        | x::xs ->  let p = walk (0, 0) x;
                    if p = (3, 3) then 
                        let l = x |> List.length;
                        if l > o then
                            dfs k l xs;
                        else 
                            dfs k o xs;
                    else
                        let ns = moves k x p;
                        dfs k o (ns @ xs);

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0];

    search input [[]]
    |> printfn "Day 17, part 1: %s";

    dfs input 0 [[]]
    |> printfn "Day 17, part 2: %d";