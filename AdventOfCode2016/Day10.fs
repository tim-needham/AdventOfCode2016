module Day10

open System;
open System.IO;

type Bot = { Id : int; Chips : int list; };;

type Output = { Id : int; Chips : int list; };;

type Target =
    | Output of x : int
    | Bot of x : int;;

type Instruction =
    | Input of v : int * b : int
    | Spread of b : int * l : Target * h : Target;;

let parseDest (d : string) (l : string) =
    match d with
        | "bot" -> Bot (Int32.Parse l);
        | "output" -> Output (Int32.Parse l);

let parse (s : string) : Instruction =
    match s.Split(' ') with
        | [| "value"; v; _; _; _; b |] -> Input (Int32.Parse v, Int32.Parse b);
        | [| "bot"; b; _; _; _; p; q; _; _; _; x; y |] -> Spread (Int32.Parse b, parseDest p q, parseDest x y);

let upsertBot (b : int) (v : int) (bs : Bot list) : Bot list =
    match List.tryFindIndex (fun (a : Bot) -> a.Id = b) bs with
        | Some _ -> [for z in bs -> if z.Id = b then { Id = z.Id; Chips = v :: z.Chips } else z ];
        | None -> { Id = b; Chips = [v]; } :: bs;

let upsertOutput (o : int) (v : int) (os : Output list) : Output list =
    match List.tryFindIndex (fun (a : Output) -> a.Id = o) os with
        | Some _ -> [for z in os -> if z.Id = o then { Id = z.Id; Chips = v :: z.Chips } else z ];
        | None -> { Id = o; Chips = [v]; } :: os;

let applyVal (i : Instruction) (b : Bot list) : Bot list =
    match i with
        | Spread (_, _, _) -> b;
        | Input (x, y) -> upsertBot y x b;

let distribute (i : Instruction) (c : int) (d : int) (b : Bot list) (o : Output list) : Bot list * Output list =
    match i with
        | Spread (n, Bot x, Bot y) -> (b |> upsertBot x c |> upsertBot y d , o);
        | Spread (n, Bot x, Output y) -> (b |> upsertBot x c, o |> upsertOutput y d);
        | Spread (n, Output x, Bot y) -> (b |> upsertBot y d, o |> upsertOutput x c);
        | Spread (n, Output x, Output y) -> (b, o |> upsertOutput x c |> upsertOutput y d);
        | Input (x, y) -> (b, o);

let rec find (b : int) (i : Instruction list) : Instruction =
    match i with
        | [] -> failwith "No instruction found!";
        | Input (_, _)::xs -> find b xs;
        | Spread (x, y, z)::xs ->  if x = b then Spread (x, y, z) else find b xs;

let rec propagate (i : Instruction list) (f : int) (b : Bot list) (o : Output list) : int * Output list =
    match b |> Seq.filter (fun x -> x.Chips |> List.length > 1) |> Seq.toList with
        | [] -> (f, o);
        | bot::xs ->    let chips = bot.Chips
                                    |> List.sort;

                        let g = if chips = [17; 61] then bot.Id else f;
                        let p = chips |> List.head;
                        let q = chips |> List.tail |> List.head;
                        let instr = find bot.Id i;
                        let bs = [for c in b -> if c.Id = bot.Id then { Id = bot.Id; Chips = [] } : Bot else c];
                        let (d, e) = distribute instr p q bs o;
                        propagate i g d e;
    
let run (file : string) =
    let input = Seq.toList (File.ReadLines(file));

    let instructions = Seq.map (fun x -> parse x) input
                        |> Seq.toList;

    let state = Seq.fold (fun a c -> applyVal c a) [] instructions;

    // This only works because, by inspection, we know only one bot at a time has two chips???
    let (b, o) = propagate instructions 0 state [];

    b
    |> printfn "Day 10, part 1: %d";

    o
    |> Seq.filter (fun x -> x.Id = 0 || x.Id = 1 || x.Id = 2)
    |> Seq.fold (fun a c -> a * (c.Chips |> List.head)) 1
    |> printfn "Day 10, part 2: %d";


