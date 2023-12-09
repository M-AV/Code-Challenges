module _2023_06

open InputProvider
open Calculations
open Parsing
open System
open Xunit

let parseInput_part1 (input : string seq) = 
    let numbers = 
        input
        |> Seq.map (fun x -> x.Split ':')
        |> Seq.map (fun x -> x[1].Trim())
        |> Seq.map (fun x -> x.Split ' ' |> Array.filter (fun x -> x.Length > 0) |> Array.map int64)
        |> Array.ofSeq

    Array.zip numbers[0] numbers[1]

let parseInput_part2 (input : string seq) = 
    let numbers = 
        input
        |> Seq.map (fun x -> x.Split ':')
        |> Seq.map (fun x -> x[1].Trim())
        |> Seq.map (fun x -> x.Replace(" ", ""))
        |> Seq.map (fun x -> x.Split ' ' |> Array.filter (fun x -> x.Length > 0) |> Array.map int64)
        |> Array.ofSeq

    Array.zip numbers[0] numbers[1]


let solve (input:(int64 * int64) array) =
    let calculate (time, distance) = 
        [1L .. (time-1L)]
        |> List.map (fun x -> x * (time - x))
        |> List.filter (fun x -> x > distance)
        |> List.length
    
    input
    |> Array.map calculate
    |> Array.fold (fun x i -> x * i) 1

let execute (input : string seq) =
    let part1_res = solve (parseInput_part1 input)

    let part2_res = solve (parseInput_part2 input)

    part1_res.ToString(), part2_res.ToString()

[<Fact>] // Takes ~10 seconds for part 2
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "6" |> Async.RunSynchronously)
    Assert.Equal("449550", part1)
    Assert.Equal("28360140", part2)