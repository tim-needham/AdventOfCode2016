module Day12

open System;
open System.IO;

type Pointer =
    | Val of x : int
    | Reg of x : string;;

type Instruction =
    | Cpy of x : Pointer * y : Pointer
    | Inc of x : Pointer
    | Dec of x : Pointer
    | Jnz of x : Pointer * y : int

type Register = { Id : string; Val : int };;

let parse (s : string) : Instruction =
    match s.Split(' ') with
        | [| "cpy"; x; y |] ->  match Int32.TryParse x with
                                | true, n -> Cpy (Val n, Reg y);
                                | false, _ -> Cpy (Reg x, Reg y);
        | [| "inc"; x |] -> Inc (Reg x);
        | [| "dec"; x |] -> Dec (Reg x);
        | [| "jnz"; x; y |] ->  let o = Int32.Parse y;
                                match Int32.TryParse x with
                                | true, n -> Jnz (Val n, o);
                                | false, _ -> Jnz (Reg x, o);

let rec eval (s : string) (m : Register list) : int =
    match m with
        | [] -> 0;
        | x::xs ->  if x.Id = s then x.Val else eval s xs;

let rec compute (p : int) (i : Instruction list) (m : Register list) : Register list =
    if p >= i.Length then
        m;
    else
        match i.[p] with
            | Cpy (Val x, Reg y) -> compute (p + 1) i [for r in m -> if r.Id = y then { Id = y; Val = x } else r];
            | Cpy (Reg x, Reg y) -> let n = eval x m;
                                    compute (p + 1) i [for r in m -> if r.Id = y then { Id = y; Val = n } else r];
            | Inc (Reg x) -> compute (p + 1) i [for r in m -> if r.Id = x then { Id = x; Val = r.Val + 1 } else r];
            | Dec (Reg x) -> compute (p + 1) i [for r in m -> if r.Id = x then { Id = x; Val = r.Val - 1 } else r];
            | Jnz (Reg x, y) -> if eval x m <> 0 then compute (p + y) i m else compute (p + 1) i m;
            | Jnz (Val x, y) -> if x <> 0 then compute (p + y) i m else compute (p + 1) i m;

let run (file : string) =
    let instructions = Seq.toList (File.ReadLines(file))
                        |> Seq.map (fun x -> parse x)
                        |> Seq.toList;

    [ { Id = "a"; Val = 0 }; { Id = "b"; Val = 0 }; { Id = "c"; Val = 0 }; { Id = "d"; Val = 0 } ]
    |> compute 0 instructions
    |> eval "a"
    |> printfn "Day 12, part 1: %d";

    [ { Id = "a"; Val = 0 }; { Id = "b"; Val = 0 }; { Id = "c"; Val = 1 }; { Id = "d"; Val = 0 } ]
    |> compute 0 instructions
    |> eval "a"
    |> printfn "Day 12, part 2: %d";

