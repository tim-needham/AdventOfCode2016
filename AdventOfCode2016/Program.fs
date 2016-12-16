// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System;

[<EntryPoint>]
let main argv = 

    let mutable cont = true;

    let problems = 
        [| 
            Day1.run; Day2.run; Day3.run; Day4.run; Day5.run; Day6.run; Day7.run;
            Day8.run; Day9.run; Day10.run; Day11.run; Day12.run; Day13.run; Day14.run;
            Day15.run; Day16.run
        |];

    while cont do
        printfn "Enter a day number (1-25) to run that day's challenge.";
        let i = System.Console.ReadLine();

        match Int32.TryParse(i) with
            | true, n -> problems.[n-1] (String.Format(@".\day{0}.txt", n));
            | false, _ -> cont <- false;

    0 // return an integer exit code
