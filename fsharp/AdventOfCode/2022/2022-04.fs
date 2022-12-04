module _2022_4

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: In each pair of ranges, how many are fully covered by the other
// Task 2: Find all that has some overlap

type SectionRange = int * int
type Pair = SectionRange * SectionRange

let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> x.Split(',', '-') |> Array.map int) 
    |> Seq.map (fun x -> Pair(SectionRange(x[0], x[1]), SectionRange(x[2], x[3])))

let hasCompleteOverlap (pair:Pair) =
    (fst (fst pair) <= fst (snd pair) && snd (fst pair) >= snd(snd pair)) ||
    (fst (fst pair) >= fst (snd pair) && snd (fst pair) <= snd(snd pair))

let part1 input =
    input
    |> Seq.map hasCompleteOverlap
    |> Seq.filter id
    |> Seq.length

let part2 input = 
    let hasOverlap (pair:Pair) =
        hasCompleteOverlap(pair) ||
        (fst (fst pair) <= fst (snd pair) && snd (fst pair) >= fst (snd pair)) ||
        (fst (snd pair) <= fst (fst pair) && snd (snd pair) >= fst (fst pair))

    input
        |> Seq.map hasOverlap
        |> Seq.filter id
        |> Seq.length

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "4" |> Async.RunSynchronously)
    Assert.Equal("657", part1)
    Assert.Equal("938", part2)