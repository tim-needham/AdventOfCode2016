module Day11

open System;
open System.IO;

type Component = 
    | Chip of l : string
    | Generator of l : string;;
       
type Floor = { Id : int; Components : Component list };;

type Lab = { Lift : int; Floors : Floor list };;

let floors = [""; "first"; "second"; "third"; "fourth";]

let parse (s : string) : Floor =
    match s.Split([|' '; ','; '.'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | "The" :: n :: "floor" :: "contains" :: "nothing" :: ["relevant"] ->   let x = List.findIndex (fun a -> a = n) floors;
                                                                                { Id = x; Components = [] };
        | "The" :: n :: "floor" :: "contains" :: xs ->  let x = List.findIndex (fun a -> a = n) floors;
                                                        let comps = xs 
                                                                    |> Seq.where (fun a -> a <> "a" && a <> "generator" && a <> "microchip" && a <> "and")
                                                                    |> Seq.map (fun a -> a.Split('-') |> Array.toList)
                                                                    |> Seq.map (fun a -> a |> List.head |> if a.Length > 1 then Chip else Generator)
                                                                    |> Seq.toList;
                                                        { Id = x; Components = comps};
                                                           
let success (l : Lab) : bool =
    match l.Floors.[0].Components, l.Floors.[1].Components, l.Floors.[2].Components with
        | [], [], [] -> true;
        | _ -> false;                                                        

let rec remove (n : Component) (i : Component list) : Component list =
    match i with
        | [] -> [];
        | x::xs when x = n -> xs;
        | y::xs -> y :: (remove n xs);
        
let rec single (o : Component list) (i : Component list) : (Component list * Component list) list =
    match i with
        | [] -> [];
        | x::xs -> ([x], o @ xs) :: single (o @ [x]) xs;
               
let rec double (o : Component list) (i : Component list) : seq<Component list * Component list> =
    seq {
        match i with 
            | [] -> ignore [];
            | x::xs ->  for y in xs do yield (x :: [y], o @ (remove y xs));
                        yield! double (o @ [x]) xs;
    };

let valid (c : Component list) : bool =
    match c with
        | [] -> true;
        | x::xs ->  let gs = c 
                            |> Seq.choose (fun a -> match a with
                                                    | Generator c -> Some (Generator c);
                                                    | _ -> None)
                            |> Seq.toList;

                    if gs = [] then
                        true;
                    else
                        let cs = c 
                                |> Seq.choose (fun a -> match a with
                                                        | Chip c -> Some (Generator c);
                                                        | _ -> None)
                                |> Seq.toList;

                        let ds = Seq.fold (fun a b -> remove b a) cs gs;
                        ds = [];
        
let cargo (i : Component list) : (Component list * Component list) list =
    match i with
        | [] -> [];
        | [x] -> single [] i;
        | x::xs ->  single [] i 
                    @ (double [] i |> Seq.toList)
                    |> Seq.filter (fun (a, b) -> valid b)
                    |> Seq.toList;

let moves (n : int) (l : Lab) : (int * Lab) list = 
    let f = l.Floors.[l.Lift - 1];
    let c = cargo f.Components;
    let o = if l.Lift < 4 then [(l.Lift + 1, l.Floors.[l.Lift].Components)] else [] 
            @ if l.Lift > 1 && l.Floors.[l.Lift - 1].Components <> [] then [(l.Lift - 1, l.Floors.[l.Lift - 2].Components)] else [];
    [for (a, b) in o do for (d, e) in c do yield (a, b, d, e)]
    |> Seq.where (fun (a, b, d, e) -> valid (b@d))
    |> Seq.map (fun (a, b, d, e) -> { Lift = a; Floors = [for i in 1..4 -> if i = l.Lift then { Id = i; Components = e } else if i = a then { Id = i; Components = (b@d) } else l.Floors.[i-1]]})
    |> Seq.map (fun m -> (n, m))
    |> Seq.toList;

let rec bfs (e : Lab list) (l : (int * Lab) list) : int =
    match l with
        | [] -> 0;
        | (n, x)::xs ->  if success x then
                            n;
                         else
                            let es = x::e;
                            let ms = moves (n+1) x;
                            let os = ms
                                    |> Seq.filter (fun (i, a) -> es |> List.tryFindIndex (fun b -> b = a) = None)
                                    |> Seq.toList; 
                            
                            bfs es (xs @ os);  

let run (file : string) =
    let input = Seq.toList (File.ReadLines(file));

    let floors = Seq.map (fun x -> parse x) input
                |> Seq.toList;

    let lab =  { Lift = 1; Floors = floors };

    let v = bfs [] [(1, lab)];

    0
    |> printfn "Day 11, part 1: %d";

    0
    |> printfn "Day 11, part 2: %d";




