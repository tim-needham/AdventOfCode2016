module Day16

open System.IO;

let dragon (bs : bool list) : bool list =
    bs @ (false :: (bs |> List.rev |> List.map (fun x -> not x)));

let rec pad (n : int) (bs : bool list) : bool list =
    if bs |> List.length >= n then
        bs |> Seq.take n |> Seq.toList;
    else
        pad n (dragon bs);

let rec check (bs : bool list) : bool list =
    bs
    |> Seq.pairwise
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (i, _) -> i % 2 = 0)
    |> Seq.map (fun (_, (x, y)) -> x = y)
    |> Seq.toList;

let rec checkSum (bs : bool list) : bool list =
    match bs |> List.length with
        | n when n % 2 = 0 -> checkSum (check bs);
        | _  -> bs;

let run (file : string) =
    let input = (Seq.toList (File.ReadLines(file))).[0]
                |> Seq.toList
                |> List.map (fun x -> x = '1');
    
    pad 272 input 
    |> checkSum
    |> List.map (fun x -> if x then '1' else '0')
    |> Seq.fold (fun a c -> a + string c) ""
    |> printfn "Day 16, part 1: %s";

    pad 35651584 input 
    |> checkSum
    |> List.map (fun x -> if x then '1' else '0')
    |> Seq.fold (fun a c -> a + string c) ""
    |> printfn "Day 16, part 2: %s";