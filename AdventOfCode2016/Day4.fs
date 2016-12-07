module Day4

open System;
open System.IO;

let parse (s : string) : (string * char list * int * string) =
    let parts = s.Split([| '['; ']'; '-' |], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.toList
                |> List.rev
                |> List.toSeq;

    let checksum = parts
                    |> Seq.head;
    let sector = parts
                    |> Seq.skip 1
                    |> Seq.head
                    |> Int32.Parse;
    let text = parts
                    |> Seq.skip 2
                    |> Seq.toList
                    |> List.rev;
    let letters = text
                    |> List.map (fun x -> [for c in x -> c])
                    |> List.fold (fun a c -> a @ c) [];
    let name = text
                    |> List.fold (fun a c -> a + (if a = "" then "" else "-") + c) "";
    (name, letters, sector, checksum);

let rec merge (c : char) (t : (char * int) list) : (char * int) list =
    match t with
        | [] -> [(c, 1)];
        | (x, n)::xs -> if c = x then
                            (x, n + 1) :: xs;
                        else
                            (x, n) :: merge c xs;

let rec accrete (t : (char * int) list) (c : char list) : (char * int) list =
    match c with
        | [] -> t;
        | x::xs -> accrete (merge x t) xs;

let checksum (c : char list) : string =
    c
    |> accrete []
    |> Seq.sortBy (fun (x, y) -> -y, x)
    |> Seq.map (fun (x, y) -> x)
    |> Seq.take 5
    |> Seq.toList
    |> String.Concat;

let shift (c : char) (v : int) : char =
    match c with
        | '-' -> ' ';
        | _ ->  let i = (int c) - (int 'a');
                let o = (i + v) % 26;
                char (o + (int 'a'));

let rec decode (o : string) (v : int) (s : string) : string =
    match s with
        | "" -> o;
        | ss -> decode (o + string (shift ss.[0] v)) v ss.[1..];

let run (file : string) =
    let lines = Seq.toList (File.ReadLines(file));

    let rooms = lines
                |> Seq.map (fun x -> parse x)
                |> Seq.where (fun (n, l, s, c) -> checksum l = c);

    rooms
    |> Seq.map (fun (n, l, s, c) -> s)
    |> Seq.fold (fun a c -> a + c) 0
    |> printfn "Day 4, part 1: %d";

    rooms
    |> Seq.where (fun (n, l, s, c) -> decode "" s n = "northpole object storage")
    |> Seq.map (fun (n, l, s, c) -> s)
    |> Seq.head
    |> printfn "Day 4, part 2: %d";

