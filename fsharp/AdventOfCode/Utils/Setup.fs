module Setup

open System
open System.IO

let createFile year (day:int) (template:string) =
    let formattedDay = day.ToString("D2")
    let fileName = $"./../../../AdventOfCode/{year}/{year}-{formattedDay}.fs"
    let modifiedTemplate = template.Replace("dd", day.ToString()).Replace("DD", day.ToString("D2"))
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
        printfn "    | (\"%s\", \"%i\") -> Some _%s_%s.execute" year i year (i.ToString("D2"))

    printfn ""
    printfn "Copy pasta the lines into the csproj file"

    for i in 1 .. 25 do
        printfn "    <Compile Include=\"AdventOfCode\%s\%s-%s.fs\" />" year year (i.ToString("D2"))


    0