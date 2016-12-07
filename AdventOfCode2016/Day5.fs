module Day5

open System;
open System.IO;
open System.Security.Cryptography;
open System.Text;

let md5 (d : byte array) : byte array =
    use md5 = MD5.Create();
    md5.ComputeHash(d);

let bytesToHex (b : byte array) : string =
    b
    |> Seq.map (fun x -> String.Format("{0:x2}", x))
    |> String.Concat;

let rec find (n : int) (k : string) : int * string =
    let b = md5 (Encoding.ASCII.GetBytes(k + n.ToString()));
    match b.[0] with
        | 0uy ->    match b.[1] with
                        | 0uy ->    match b.[2] with
                                        | x when x < 16uy -> (n, bytesToHex b)
                                        | _ -> find (n+1) k;
                        | _ -> find (n+1) k;
        | _ -> find (n+1) k;                                    

let rec findAll (i : int) (n : int) (k : string) (s : string list) : string list =
    match i with
        | 0 -> s;
        | j ->  let (n', t) = find n k;
                findAll (j-1) (n'+1) k [t]@s;

let rec findOrdered (i : int) (n : int) (k : string) (s : (int * string) list) : (int * string) list =
    match List.length s with
        | z when z = i -> s;
        | _ ->  let (n', t) = find n k;
                let b, p = Int32.TryParse (t.Substring(5, 1));
                match b with
                    | false ->  findOrdered i (n'+1) k s;
                    | true ->   if p < i then
                                    match List.tryFindIndex (fun x -> fst x = p) s with 
                                        | Some j -> findOrdered i (n'+1) k s;
                                        | None -> findOrdered i (n'+1) k ([(p, t.Substring(6, 1))]@s);
                                else
                                    findOrdered i (n'+1) k s;

let run (file : string) =
    let key = (Seq.toList (File.ReadLines(file))).[0];

    findAll 8 1 key []
    |> Seq.map (fun x -> x.Substring(5, 1))
    |> Seq.toList
    |> List.rev
    |> String.Concat
    |> printfn "Day 5, part 1: %A";

    findOrdered 8 1 key []
    |> Seq.sortBy (fun (p, n) -> p)
    |> Seq.map (fun (p, n) -> n)
    |> String.Concat
    |> printfn "Day 5, part 2: %A";