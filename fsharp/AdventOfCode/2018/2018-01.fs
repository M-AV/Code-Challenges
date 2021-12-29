module _2018_01

open InputProvider
open System
open Xunit

// Task 1: Sum all numbers
// Task 2: Find first duplicate frequency - repeat input if necessary

let parseInput (input : string seq) = 
    input |> Seq.map Int32.Parse |> List.ofSeq

let rec findFirstDuplicate seen current remaining all =
    match remaining with
    | head::tail ->
        let newPos = current + head
        if Set.contains newPos seen then
            newPos
        else
            findFirstDuplicate (Set.add newPos seen) newPos tail all
    | [] -> findFirstDuplicate seen current all all

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = parsed |> Seq.sum

    let part2 = findFirstDuplicate (Set.empty.Add 0) 0 parsed parsed

    part1.ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2018" "1" |> Async.RunSynchronously)
    Assert.Equal("470", part1)
    Assert.Equal("790", part2)