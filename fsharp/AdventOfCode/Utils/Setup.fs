module Setup

open System
open System.IO

let createFile year day (template:string) =
    let fileName = $"./../../../AdventOfCode/{year}/{year}-{day}.fs"
    let modifiedTemplate = template.Replace("dd", day)
    let s = 
        match File.Exists(fileName) with 
        | true -> 
            printfn "%s-%s already exists.. Skipping!" year day
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
        let formattedDay = i.ToString("D2")
        //printfn "%s" formattedDay
        createFile year formattedDay template |> ignore

    printfn "Files created..";
    printfn "Copy pasta the code below to the Program.fs file"
    printfn ""

    for i in 1 .. 25 do
        let formattedDay = i.ToString("D2")
        printfn "    | (\"%s\", \"%s\") -> Some _%s_%s.execute" year formattedDay year formattedDay


    0