module _2021_7

open InputProvider
open Xunit

// Task 1: Find minimum number of adjustments crabs need to make to be aligned
// Task 2: Find minimum number but fuel is exponential

let parseInitialState (line:string) =
    line.Split(',') |> Array.map int

let findMinimumMoves positions fuelCostFunction =
    let fuelNeeded positions target =
        positions |> 
        List.map (fun x -> (max target x) - (min target x)) |> 
        List.map fuelCostFunction |>
        List.sum

    let lowest = List.min positions
    let highest = List.max positions

    [ for i in lowest .. highest -> i ] |>
        List.map (fun x -> fuelNeeded positions x) |>
        List.min

let execute (input : string seq) =
    
    let parsed = 
        input |> 
        Seq.map parseInitialState |> List.ofSeq |> List.head |> List.ofSeq |> List.sort

    printfn "Input count: %i" (Seq.length parsed)

    let part1 = findMinimumMoves parsed id

    // Formula: 0.5 * n * (n + 1)
    let part2 = findMinimumMoves parsed (fun x -> (x * (x + 1)) >>> 1)

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2021" "7" |> Async.RunSynchronously)
    Assert.Equal("349357", part1)
    Assert.Equal("96708205", part2)