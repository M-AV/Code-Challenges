module _2023_09

open InputProvider
open Calculations
open Parsing
open System
open Xunit

let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> x.Split ' ')
    |> Seq.map (fun x -> x |> Array.map int64)
    |> List.ofSeq

let part1 input =
    let rec processLine (line:int64 array) =
        let nonZeroCount = line |> Array.filter (fun x -> x <> 0L) |> Array.length
        if nonZeroCount = 0 then
            0L
        else
            let newLine = 
                line 
                |> Array.pairwise
                |> Array.map (fun (x,y) -> y - x)

            let subRes = processLine newLine
            line[line.Length - 1] + subRes

    input
    |> List.map processLine
    |> List.sum

let part2 input =
    let rec processLine (line:int64 array) =
        let nonZeroCount = line |> Array.filter (fun x -> x <> 0L) |> Array.length
        if nonZeroCount = 0 then
            0L
        else
            let newLine = 
                line 
                |> Array.pairwise
                |> Array.map (fun (x,y) -> y - x)

            let subRes = processLine newLine
            line[0] - subRes

    input
    |> List.map processLine
    |> List.sum

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "9" |> Async.RunSynchronously)
    Assert.Equal("1696140818", part1)
    Assert.Equal("1152", part2)