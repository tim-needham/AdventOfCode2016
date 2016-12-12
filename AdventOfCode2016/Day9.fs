module Day9

open System;
open System.IO;

type Chunk =
    | Token of t : string
    | Inflate of n : int * c : Chunk list;;
       
let rec build (o : Chunk list) (f : string) (m : string -> Chunk * string) (i : string) : Chunk list =
    match i.Length, f with 
        | 0, "" -> o;
        | 0, t -> o @ [Token t];
        | n ->  let x, xs = i.Substring(0, 1), i.Substring(1);
                match x, f with
                    | "(", "" ->    let t, p = m xs;
                                    build (o @ [t]) "" m p; 
                    | "(", _ -> let t, p = m xs;
                                build (o @ (Token f :: [t])) "" m p; 
                    | _, _ -> build o (f + x) m xs;
and make (i : string) : Chunk * string =
    let b = i.IndexOf(")");
    let m = i.Substring(0, b);
    let p = m.Split('x');
    let r = Int32.Parse p.[0];
    (Inflate (Int32.Parse p.[1], [Token (i.Substring(b + 1, r))]), i.Substring(b + r + 1)); 
and makeTree (i : string) : Chunk * string =
    let b = i.IndexOf(")");
    let m = i.Substring(0, b);
    let p = m.Split('x');
    let r = Int32.Parse p.[0];
    (Inflate (Int32.Parse p.[1], build [] "" (makeTree) (i.Substring(b + 1, r))), i.Substring(b + r + 1)); 

let rec size (c : Chunk list) : Int64 =
    match c with
        | [] -> 0L;
        | Token t :: xs -> (int64 t.Length) + size xs;
        | Inflate (i, x) :: xs -> ((int64 i) * size x) + size xs;
         
let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0];


    build [] "" make input                 
    |> size
    |> printfn "Day 9, part 1: %d";

    build [] "" makeTree input                
    |> size
    |> printfn "Day , part 2: %d";
