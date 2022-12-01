module Setup

open System
open System.IO

let createFile year (day:int) (template:string) =
    let formattedDay = day.ToString("D2")
    let fileName = $"./../../../AdventOfCode/{year}/{year}-{formattedDay}.fs"
    let modifiedTemplate = template.Replace("dd", day.ToString())
    let s = 
        match File.Exists(fileName) with 
        | true -> 
            printfn "%s-%i already exists.. Skipping!" year day
            0
        | false -> 
            File.WriteAllText (fileName, modifiedTemplate)
            1

    fileName

let setupYear () =
    printfn "Enter year:"
    let year = Console.ReadLine()

    let template = File.ReadAllText("./AdventOfCode/20yy-dd.fs").Replace("yyyy", year);

    match Directory.Exists($"./../../../AdventOfCode/{year}") with
        | false -> Directory.CreateDirectory $"./../../../AdventOfCode/{year}" |> ignore
        | _ -> ()

    for i in 1 .. 25 do
        createFile year i template |> ignore

    printfn "Files created..";
    printfn "Copy pasta the code below to the Program.fs file"
    printfn ""

    for i in 1 .. 25 do
        printfn "    | (\"%s\", \"%i\") -> Some _%s_%i.execute" year i year i


    0