module _2017_01

open InputProvider
open Parsing
open System
open Xunit

// Task 1: Sum digits that match the next digit in the sequence
// Task 2: Sum digits that match the digit halfway into the sequence

let parseInput (input : string seq) = 
    input |> Seq.map ints |> Seq.head

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = 
        parsed 
        |> Seq.fold (fun (agg, prev) cur -> 
            if cur = prev then
                (agg + prev, cur)
            else
                (agg, cur)) (0, parsed[parsed.Length - 1])

    let offset = parsed.Length / 2
    let part2 = 
        parsed
        |> Array.indexed
        |> Array.map (fun (idx, value) -> 
            let curOffset = 
                if offset + idx >= parsed.Length then offset + idx - parsed.Length 
                else offset + idx
            if value = parsed[curOffset] then
                value
            else
                0)
        |> Array.sum

    (fst part1).ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2017" "1" |> Async.RunSynchronously)
    Assert.Equal("1150", part1)
    Assert.Equal("1064", part2)