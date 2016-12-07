module Day7

open System.IO;

let rec parse (b : bool) (c : char list) (s : string) (o : string list) (i : string list) : (string list * string list) =
    match c with
        | [] -> match s with
                | "" -> (o |> List.rev, i |> List.rev);
                | _ ->  match b with
                        | true -> (o |> List.rev, s::i |> List.rev);
                        | false -> (s :: o |> List.rev, i |> List.rev);
        | x::xs ->  match x with
                    | '[' | ']' ->  match b with
                                    | true -> parse false xs "" o (s::i);
                                    | false -> parse true xs "" (s::o) i;
                    | _ -> parse b xs (s + string x) o i;

let rec abba (c : char list) (b : bool) (f : char) (s : char) : bool =
    match c with
    | [] -> false;
    | x::xs ->  if f = s then
                    abba xs false s x;
                else
                    match b with
                    | false ->  if x = s then
                                    abba xs true f s;
                                else
                                    abba xs false s x;
                    | true ->   if x = f then
                                    true;
                                else
                                    abba xs false s x;

let rec iter (s : string list) : bool =
    match s with
        | [] -> false;
        | x::xs ->  let c = x |> Seq.toList;
                    let (f, r) = c |> List.head, c |> List.tail;
                    let (s, t) = r |> List.head, r |> List.tail;
                    match abba t false f s with
                        | true  -> true;
                        | false -> iter xs;

let rec aba (c : char list) (f : char) (s : char) (m : (char * char) list) : (char * char) list =
    match c with
        | [] -> m |> List.rev;
        | x::xs ->  if f = s then
                        aba xs s x m;
                    else
                        if f = x then
                            aba xs s x ((f, s) :: m);
                        else
                            aba xs s x m;

let rec accrete (s : string list) (a : (char * char) list) : (char * char) list =
    match s with
        | [] -> a |> List.rev;
        | x::xs ->  let c = x |> Seq.toList;
                    let (f, r) = c |> List.head, c |> List.tail;
                    let (s, t) = r |> List.head, r |> List.tail;
                    match aba t f s [] with
                        | [] -> accrete xs a;
                        | p -> accrete xs (p@a);

let rec bab (a : (char * char) list) (b : (char * char) list) : bool =
    match a with
        | [] -> false;
        | (p, q)::xs -> match b |> List.tryFindIndex (fun (r, s) -> r = q && s = p) with
                        | Some _ -> true;
                        | None -> bab xs b;

let run (file : string) =
    let lines = Seq.toList (File.ReadLines(file));

    let ips = lines
                |> Seq.map (fun x -> parse false (x |> Seq.toList) "" [] []);

    ips
    |> Seq.where (fun (o, i) -> iter o &&  not (iter i))
    |> Seq.length
    |> printfn "Day 7, part 1: %d";

    ips
    |> Seq.map (fun (a, b) -> (accrete a [], accrete b []))
    |> Seq.where (fun (a, b) -> (a |> Seq.length > 0) && (b |> Seq.length > 0))
    |> Seq.where (fun (a, b) -> bab a b)
    |> Seq.length
    |> printfn "Day 7, part 2: %d";



