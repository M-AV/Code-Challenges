module _2015_03

open InputProvider
open System
open Xunit

// Task 1: Follow directions and count how many distinct places we see
// Task 2: How many distinct places if two entities follow every second direction

let parseInput (input : string seq) = 
    input |> Seq.head |> List.ofSeq

let newLoc (x,y) direction =
    match direction with
    | '<' -> (x-1, y)
    | '^' -> (x, y-1)
    | 'v' -> (x, y+1)
    | '>' -> (x+1, y)

// Keep track of seen locations and current position
// while recursively iterating through the directions
let rec findDistinctLocs seenLocs (x,y) directions =
    match directions with
    | head::tail -> 
        let newLoc = newLoc (x,y) head
        let updatedSet = seenLocs |> Set.add newLoc
        findDistinctLocs updatedSet newLoc tail
    | [] -> seenLocs

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let initialLoc = Set.empty.Add (0,0)

    let part1 = findDistinctLocs initialLoc (0,0) parsed

    let santasDirections = 
        parsed |> List.indexed
        |> List.filter (fun (idx, value) -> idx % 2 = 1)
        |> List.map snd
        |> findDistinctLocs initialLoc (0,0)
    let robotDirections =
        parsed |> List.indexed
        |> List.filter (fun (idx, value) -> idx % 2 = 0)
        |> List.map snd
        |> findDistinctLocs initialLoc (0,0)
    let part2 = 
        santasDirections
        |> Set.union robotDirections

    part1.Count.ToString(), part2.Count.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "3" |> Async.RunSynchronously)
    Assert.Equal("2572", part1)
    Assert.Equal("2631", part2)