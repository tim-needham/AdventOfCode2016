module Day2

open System.IO;

let move (d : char) (p : int * int) : int * int =
    let (x, y) = p;
    match d with
        | 'U' -> if y = 0 then p else (x, y - 1);
        | 'D' -> if y = 2 then p else (x, y + 1);
        | 'L' -> if x = 0 then p else (x - 1, y);
        | 'R' -> if x = 2 then p else (x + 1, y);
        | _ -> p;

let moveD (d : char) (p : int * int) : int * int =
    let (x, y) = p;
    match d with
        | 'U' -> if x - y = 2 || x + y = 2 then p else (x, y - 1);
        | 'D' -> if x - y = -2 || x + y = 6 then p else (x, y + 1);
        | 'L' -> if x + y = 2 || x - y = -2 then p else (x - 1, y);
        | 'R' -> if x + y = 6 || x - y = 2 then p else (x + 1, y);
        | _ -> p;


let rec traverse (p : int * int) (s : char list) (m : char -> (int * int) -> (int * int)) : int * int =
    match s with
        | [] -> p;
        | x::xs ->  traverse (m x p) xs m;

let find (p : (int * int) list) (s : string) (m : char -> (int * int) -> (int * int)) : (int * int) list =
    [traverse (Seq.head p) (Seq.toList s) m] @ p ;

let decode (p : (int * int)) (k : char list list) : char =
    let (x, y) = p;
    k.[y].[x];

let run (file : string) =
    let moves = Seq.toList (File.ReadLines(file));
    let square = [ [ '1'; '2'; '3' ]; [ '4'; '5'; '6' ]; [ '7'; '8'; '9' ] ];
    let diamond = [ [ ' '; ' '; '1'; ' '; ' ' ]; [ ' '; '2'; '3'; '4'; ' ' ]; [ '5'; '6'; '7'; '8'; '9' ]; [ ' '; 'A'; 'B'; 'C'; ' ' ]; [ ' '; ' '; 'D'; ' '; ' ' ] ];

    moves
    |> Seq.fold (fun a c -> find a c move) [(1, 1)]
    |> Seq.fold (fun a c -> [(decode c square)] @ a) []
    |> Seq.skip 1
    |> Seq.fold (fun a c -> a + string c) ""
    |> printfn "Day 2, part 1: %A";

    moves
    |> Seq.fold (fun a c -> find a c moveD) [(0, 2)]
    |> Seq.fold (fun a c -> [(decode c diamond)] @ a) []
    |> Seq.skip 1
    |> Seq.fold (fun a c -> a + string c) ""
    |> printfn "Day 2, part 2: %A";