module Day14

open System;
open System.IO;
open System.Security.Cryptography;
open System.Text;

type Key = { Index : int; Hash : string };;

type Candidate = { Token : char; Value : Key };;

let md5 (d : byte array) : byte array =
    use md5 = MD5.Create();
    md5.ComputeHash(d);

let bytesToHex (b : byte array) : string =
    b
    |> Seq.map (fun x -> String.Format("{0:x2}", x))
    |> String.Concat;

let rec token (n : int) (f : char) (t : int) (c : char list) : char option =
    match c with
        | [] -> None;
        | x::xs ->  if x = f then
                        if t + 1 = n then
                            Some f;
                        else
                            token n f (t+1) xs;
                    else
                        token n x 1 xs;

let rec hash (s : string) (n : int) : string =
    match n with
        | 0 ->  s
                |> Encoding.ASCII.GetBytes
                |> md5
                |> bytesToHex;
        | _ -> hash s (n-1)
                |> Encoding.ASCII.GetBytes
                |> md5
                |> bytesToHex;

let generate (s : string) (i : int) (n : int) : string =
    hash (s + i.ToString()) n;

let rec find (c : Candidate list) (o : Key list) (s : string) (i : int) (n : int) (z : int) : Key list =
    match o.Length with
        | y when y = n -> o;
        | _ ->  let cs = c
                        |> Seq.filter (fun x -> i - x.Value.Index <= 1000)
                        |> Seq.toList;

                let h = generate s i z;
                let g = token 5 Char.MinValue 0 (h |> Seq.toList);

                let p = o @ match g with
                                | None -> [];
                                | Some t -> cs 
                                            |> Seq.filter (fun x -> x.Token = t) 
                                            |> Seq.map (fun x -> x.Value) 
                                            |> Seq.toList;
                let d = match g with
                            | None -> cs;
                            | Some t -> cs
                                        |> Seq.filter (fun x -> x.Token <> t)
                                        |> Seq.toList;

                match token 3 Char.MinValue 0 (h |> Seq.toList) with
                    | Some t -> let e = { Token = t; Value = { Index = i; Hash = h} };
                                find (e::d) p s (i+1) n z;
                    | None -> find d p s (i+1) n z;

let run (file : string) =
    let salt = (Seq.toList (File.ReadLines(file))).[0];

    find [] [] salt 0 64 0
    |> List.sortBy (fun x -> 0 - x.Index)
    |> List.map (fun x -> x.Index)
    |> List.head
    |> printfn "Day 14, part 1: %d";

    find [] [] salt 0 64 2016
    |> List.sortBy (fun x -> 0 - x.Index)
    |> List.map (fun x -> x.Index)
    |> List.head
    |> printfn "Day 14, part 2: %d";