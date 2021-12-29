module _2020_01

open InputProvider
open System
open Xunit

// Task 1: Find 2 numbers whose sum is 2020 and multiply them
// Task 2: Find 3 numbers that does the same

let parseInput (input : string seq) = 
    input |> Seq.map Int32.Parse

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let (fst, snd) = 
        parsed
        |> Seq.allPairs parsed
        |> Seq.find (fun (fst, snd) -> (fst + snd) = 2020)

    let (p2_fst, (p2_snd, p2_trd)) = 
        parsed 
        |> Seq.allPairs parsed
        |> Seq.allPairs parsed
        |> Seq.find (fun (fst, (snd, trd)) -> (fst + snd + trd) = 2020)

    (fst * snd).ToString(), (p2_fst * p2_snd * p2_trd).ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2020" "1" |> Async.RunSynchronously)
    Assert.Equal("793524", part1)
    Assert.Equal("61515678", part2)