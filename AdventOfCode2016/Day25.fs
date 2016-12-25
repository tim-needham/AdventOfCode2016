module Day25

open System;
open System.IO;

type Pointer =
    | Val of x : int
    | Reg of x : string;;

type Instruction =
    | NoOp
    | Cpy of x : Pointer * y : Pointer
    | Inc of x : Pointer
    | Dec of x : Pointer
    | Jnz of x : Pointer * y : Pointer
    | Tgl of x : Pointer
    | Mul of x : Pointer * y : Pointer * z : Pointer
    | Out of x : Pointer;;

type Register = { Id : string; Val : int };;

let parse (s : string) : Instruction =
    match s.Split(' ') with
        | [| "cpy"; x; y |] ->  match Int32.TryParse x with
                                | true, n -> Cpy (Val n, Reg y);
                                | false, _ -> Cpy (Reg x, Reg y);
        | [| "inc"; x |] -> Inc (Reg x);
        | [| "dec"; x |] -> Dec (Reg x);
        | [| "jnz"; x; y |] ->  let a = match Int32.TryParse x with
                                            | true, n -> Val n;
                                            | false, _ -> Reg x;
                                let b = match Int32.TryParse y with
                                            | true, n -> Val n;
                                            | false, _ -> Reg y;
                                Jnz (a, b);
        | [| "tgl"; x |] -> match Int32.TryParse x with
                                | true, n -> Tgl (Val n);
                                | false, _ -> Tgl (Reg x);
        | [| "out"; x |] -> match Int32.TryParse x with 
                                | true, n -> Out (Val n);
                                | false, _ -> Out (Reg x);

let rec eval (s : string) (m : Register list) : int =
    match m with
        | [] -> 0;
        | x::xs ->  if x.Id = s then x.Val else eval s xs;

let toggle (x : int) (i : Instruction list) : Instruction list =
    if x >= (i |> List.length) then
        i;
    else
        let j = i |> List.take x;
        let k = match i.[x] with
                    | Cpy (x, y) -> Jnz (x, y);
                    | Inc x -> Dec x;
                    | Dec x -> Inc x;
                    | Jnz (x, y) -> Cpy (x, y);
                    | Tgl x -> Inc x;
                    | NoOp -> NoOp;
                    | Out x -> Inc x;
        let l = if x = (i |> List.length) then
                    [];
                else
                    i |> List.skip (x + 1);

        j @ (k::l);

let rec compute (p : int) (o : int list) (i : Instruction list) (m : Register list) : bool =
    if p >= i.Length then
        false;
    else if o |> List.length = 12 then
        true;
    else
        match i.[p] with
            | Cpy (Val x, Reg y) -> compute (p + 1) o i [for r in m -> if r.Id = y then { Id = y; Val = x } else r];
            | Cpy (Reg x, Reg y) -> let n = eval x m;
                                    compute (p + 1) o i [for r in m -> if r.Id = y then { Id = y; Val = n } else r];
            | Cpy (_, Val y) -> compute (p + 1) o i m;
            | Inc (Reg x) -> compute (p + 1) o i [for r in m -> if r.Id = x then { Id = x; Val = r.Val + 1 } else r];
            | Dec (Reg x) -> compute (p + 1) o i [for r in m -> if r.Id = x then { Id = x; Val = r.Val - 1 } else r];
            | Jnz (Reg x, Val y) -> if eval x m <> 0 then compute (p + y) o i m else compute (p + 1) o i m;
            | Jnz (Val x, Val y) -> if x <> 0 then compute (p + y) o i m else compute (p + 1) o i m;
            | Jnz (Reg x, Reg y) -> let z = eval y m;
                                    if eval x m <> 0 then compute (p + z) o i m else compute (p + 1) o i m;
            | Jnz (Val x, Reg y) -> let z = eval y m;
                                    if x <> 0 then compute (p + z) o i m else compute (p + 1) o i m;
            | Tgl (Val x) ->    let j = toggle (p + x) i;
                                compute (p + 1) o j m; 
            | Tgl (Reg x) ->    let y = eval x m;
                                let j = toggle (p + y) i;
                                compute (p + 1) o j m;
            | Mul (Reg x, Reg y, Reg z) ->  let a = eval x m;
                                            let b = eval y m;
                                            compute (p + 1) o i [for r in m -> if r.Id = z then { Id = z; Val = (a*b) } else r];
            | Mul (Val x, Reg y, Reg z) ->  let b = eval y m;
                                            compute (p + 1) o i [for r in m -> if r.Id = z then { Id = z; Val = (x*b) } else r];
            | Mul (Reg x, Val y, Reg z) ->  let a = eval x m;
                                            compute (p + 1) o i [for r in m -> if r.Id = z then { Id = z; Val = (a*y) } else r];
            | Mul (Val x, Val y, Reg z) ->  compute (p + 1) o i [for r in m -> if r.Id = z then { Id = z; Val = (x*y) } else r];
            | NoOp -> compute (p + 1) o i m;
            | Out (Reg x) ->    let n = eval x m
                                if o |> List.isEmpty || n = (1 - (o |> List.head)) then
                                    compute (p + 1) (n::o) i m;
                                else
                                    false;
            | Out (Val x) ->    if o |> List.isEmpty || x = (1 - (o |> List.head)) then
                                    compute (p + 1) (x::o) i m;
                                else
                                    false;

let registers (i : int) : Register list =
    [ { Id = "a"; Val = i }; { Id = "b"; Val = 0 }; { Id = "c"; Val = 0 }; { Id = "d"; Val = 0 } ];

let rec scan (i : int) (is : Instruction list) : int =
    let rs = registers i;

    if compute 0 [] is rs then
        i;
    else
        scan (i + 1) is;

let run (file : string) =
    let instructions = Seq.toList (File.ReadLines(file))
                        |> Seq.map (fun x -> parse x)
                        |> Seq.toList;

    instructions
    |> scan 0
    |> printfn "Day 25, part 1: %d";