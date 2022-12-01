module _2022_25

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: 
// Task 2: 

let parseInput (input : string seq) = 
    input

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = "N/A"

    let part2 = "N/A"

    part1.ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "25" |> Async.RunSynchronously)
    Assert.Equal("N/A", part1)
    Assert.Equal("N/A", part2)