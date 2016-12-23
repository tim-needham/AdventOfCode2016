module Day23

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
    | Mul of x : Pointer * y : Pointer * z : Pointer;;

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
        let l = if x = (i |> List.length) then
                    [];
                else
                    i |> List.skip (x + 1);

        j @ (k::l);

let rec compute (p : int) (i : Instruction list) (m : Register list) : Register list =
    if p >= i.Length then
        m;
    else
        match i.[p] with
            | Cpy (Val x, Reg y) -> compute (p + 1) i [for r in m -> if r.Id = y then { Id = y; Val = x } else r];
            | Cpy (Reg x, Reg y) -> let n = eval x m;
                                    compute (p + 1) i [for r in m -> if r.Id = y then { Id = y; Val = n } else r];
            | Cpy (_, Val y) -> compute (p + 1) i m;
            | Inc (Reg x) -> compute (p + 1) i [for r in m -> if r.Id = x then { Id = x; Val = r.Val + 1 } else r];
            | Dec (Reg x) -> compute (p + 1) i [for r in m -> if r.Id = x then { Id = x; Val = r.Val - 1 } else r];
            | Jnz (Reg x, Val y) -> if eval x m <> 0 then compute (p + y) i m else compute (p + 1) i m;
            | Jnz (Val x, Val y) -> if x <> 0 then compute (p + y) i m else compute (p + 1) i m;
            | Jnz (Reg x, Reg y) -> let z = eval y m;
                                    if eval x m <> 0 then compute (p + z) i m else compute (p + 1) i m;
            | Jnz (Val x, Reg y) -> let z = eval y m;
                                    if x <> 0 then compute (p + z) i m else compute (p + 1) i m;
            | Tgl (Val x) ->    let j = toggle (p + x) i;
                                compute (p + 1) j m; 
            | Tgl (Reg x) ->    let y = eval x m;
                                let j = toggle (p + y) i;
                                compute (p + 1) j m;
            | Mul (Reg x, Reg y, Reg z) ->  let a = eval x m;
                                            let b = eval y m;
                                            compute (p + 1) i [for r in m -> if r.Id = z then { Id = z; Val = (a*b) } else r];
            | NoOp -> compute (p + 1) i m;

let rec scan (i : int) (j : int) (ps : Instruction list) (rs : Instruction list) (is : Instruction list) : Instruction list =
    // We ran out of things to look at :(
    if i + (ps |> List.length) > (is |> List.length) then
        is;
    else
        // Found it!
        if j = (ps |> List.length) then
            (is |> List.take i) @ rs @ (is |> List.skip (i + (ps |> List.length)));
        else
            // We're matching.
            if is.[i+j] = ps.[j] then
                scan i (j+1) ps rs is;
            // We aren't.
            else
                scan (i+1) 0 ps rs is;

let run (file : string) =
    let instructions = Seq.toList (File.ReadLines(file))
                        |> Seq.map (fun x -> parse x)
                        |> Seq.toList;

    // Sigh... another inspection problem. Scan the input for a pair of incrementing loops
    // that are effectively a big multiplication and replace them. We want to preserve 
    // instruction length so we'll need a NoOp and we'll want to zero our loop iterators
    // so look for this:
    //
    // cpy b c
    // inc a
    // dec c
    // jnz c -2
    // dec d
    // jnz d -5
    //
    // and replace with this:
    //
    // mul b d a
    // cpy 0 c
    // cpy 0 d
    // noop
    // noop
    // noop
    //
    // and hope that tgl doesn't introduce any new loops.

    let pattern = [ Cpy (Reg "b", Reg "c"); Inc (Reg "a"); Dec (Reg "c"); Jnz (Reg "c", Val -2); Dec (Reg "d"); Jnz (Reg "d", Val -5) ];
    let replace = [ Mul (Reg "b", Reg "d", Reg "a"); Cpy (Val 0, Reg "c"); Cpy (Val 0, Reg "d"); NoOp; NoOp; NoOp ];
    let matched = scan 0 0 pattern replace instructions;

    [ { Id = "a"; Val = 7 }; { Id = "b"; Val = 0 }; { Id = "c"; Val = 0 }; { Id = "d"; Val = 0 } ]
    |> compute 0 matched
    |> eval "a"
    |> printfn "Day 23, part 1: %d";

    [ { Id = "a"; Val = 12 }; { Id = "b"; Val = 0 }; { Id = "c"; Val = 0 }; { Id = "d"; Val = 0 } ]
    |> compute 0 matched
    |> eval "a"
    |> printfn "Day 23, part 2: %d";