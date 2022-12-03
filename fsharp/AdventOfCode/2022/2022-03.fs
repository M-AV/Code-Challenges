module _2022_3

open InputProvider
open Parsing
open Xunit
open System.Linq

// Task 1: Find duplicate letter in each half of each string and sum up the values (a-z = 1-26, A-Z=27-52)
// Task 2: Find common item in each set of 3 lines and sum like before

let parseInput (input : string seq) = 
    input

let convertToPoints letter =
    match (int letter) with
    | i when i < 97 -> i - 38
    | x -> x - 96

let part1 (input : string seq) =
    let findDuplicate str =
        let middle = Array.length str / 2

        let first = str |> Seq.take middle;
        let second = str |> Seq.skip middle;

        let inter = Enumerable.Intersect(first, second) |> Seq.exactlyOne

        inter

    input 
    |> Seq.map Array.ofSeq
    |> Seq.map findDuplicate
    |> Seq.map convertToPoints
    |> Seq.sum

let part2 input =
    let sum = 
        input
        |> batchesOf 3
        // Not sure this is the best way of taking the first 3 items..
        |> Seq.map (fun x -> (x |> Seq.take 1 |> Seq.exactlyOne, x |> Seq.skip 1 |> Seq.take 1 |> Seq.exactlyOne, x |> Seq.skip 2 |> Seq.take 1 |> Seq.exactlyOne))
        |> Seq.map (fun (x,y,z) -> Enumerable.Intersect(Enumerable.Intersect(x , y), z) |> Seq.exactlyOne)
        |> Seq.map convertToPoints
        |> Seq.sum

    sum

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "3" |> Async.RunSynchronously)
    Assert.Equal("8394", part1)
    Assert.Equal("2413", part2)