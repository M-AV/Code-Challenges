module _2022_7

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Parse folder structure and find every folder with size at 100000 or less and sum it.
// Task 2: Find smallest folder to delete so we get enough space

// The idea here is to parse everything into a flat file structure where each directory name includes the parent name.
// To find sub-folders we should then simply be able to check if the name starts with the parent name

let parseInput (input : string seq) = 
    let parseFile (line:string) =
        let split = line.Split ' '
        
        (split[0] |> int, split[1])
        

    let rec parse (input:string list) (currentPath:string) (folders:(string * int) list)=
        match input with
        | [] -> folders
        | head::tail -> 
            match head with 
            | Prefix "$ cd .." rest -> 
                let lastSlash = currentPath |> Seq.findIndexBack(fun x -> x = '/')
                let parentDir = currentPath[0..lastSlash - 1]
                parse tail parentDir folders
            | Prefix "$ cd " rest ->
                let newDir = currentPath + "/" + rest
                parse tail newDir folders
            | Prefix "$ ls" rest -> 
                let files = 
                    tail |> List.takeWhile (fun x -> not(x.StartsWith("$"))) 
                    
                let fileSize = files |> List.filter (fun x -> not(x.StartsWith("dir"))) |> List.map parseFile |> List.sumBy fst
                let remaining = tail |> List.skip files.Length
                let updatedFolders = ((currentPath, fileSize)::folders)

                parse remaining currentPath updatedFolders

    parse (input |> Seq.skip 1 |> List.ofSeq) "" []

let rec calcSpaceByFolder (folder:string * int) (all:(string * int) list) =
    let size = 
        all 
        |> List.filter (fun x -> (fst x).StartsWith(fst folder))
        |> List.sumBy (fun x -> snd x)

    size

let part1 (input:(string * int) list) =
    let rec solve (remaining:(string * int) list) (input:(string * int) list) =
        match remaining with
        | [] -> 0
        | head::tail ->
            let size = calcSpaceByFolder head input

            if (size <= 100000) then
                size + (solve tail input)
            else 
                solve tail input

    solve input input


let part2 input =
    let totalSpace = 70000000
    let spaceNeeded = 30000000
    let totalSpaceUsed = calcSpaceByFolder (input |> List.last) input
    let toBeDeleted = spaceNeeded - (totalSpace - totalSpaceUsed)

    input 
    |> List.map (fun x -> calcSpaceByFolder x input) 
    |> List.filter (fun x -> toBeDeleted < x) 
    |> List.min
    

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "7" |> Async.RunSynchronously)
    Assert.Equal("1477771", part1)
    Assert.Equal("3579501", part2)