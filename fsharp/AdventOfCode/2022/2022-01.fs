module _2022_1

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Sum all groups of numbers (split by empty line) and find biggest
// Task 2: After summing each group, find the sum of the 3 biggest

let parseInput (input : string seq) = 
    input  |> List.ofSeq

let solvePart1 input =
    let rec sumRecursively solution input =
        match input with
        | ""::tail -> Math.Max(solution, sumRecursively 0 tail)
        | x::tail -> 
            let parsedInt = x |> int
            sumRecursively (solution + parsedInt) tail
        | [] -> 0
    sumRecursively 0 input

let solvePart2 input = 
    // Could have used this function for the first part as well, would have made it 
    let rec sumGroups input sums currentSum =
        match input with 
        | ""::tail -> sumGroups tail (currentSum::sums) 0
        | x::tail -> 
            let parsedInt = x |> int
            sumGroups tail sums (currentSum + parsedInt)
        | [] -> sums

    let sums = sumGroups input [] 0

    sums |> List.sortByDescending (fun x -> x) |> List.take 3 |> List.sum



let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = solvePart1 parsed

    let part2 = solvePart2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "1" |> Async.RunSynchronously)
    Assert.Equal("72070", part1)
    Assert.Equal("211805", part2)