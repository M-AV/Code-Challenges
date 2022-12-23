module _yyyy_DD

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: 
// Task 2: 

let parseInput (input : string seq) = 
    input

let part1 input =
    0

let part2 input =
    0

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

//[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "yyyy" "dd" |> Async.RunSynchronously)
    Assert.Equal("N/A", part1)
    Assert.Equal("N/A", part2)