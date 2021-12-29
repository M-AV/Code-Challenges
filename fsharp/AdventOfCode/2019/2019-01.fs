module _2019_01

open InputProvider
open System
open Xunit

// Task 1: Divide each number with 3 and subtract 2, then sum
// Task 2: For each result in Task 1, repeat calculation. Keep going until all 0 (count negative as 0)

let parseInput (input : string seq) = 
    input |> Seq.map Int32.Parse

let rec calcTotalFuelRequirement sum input =
    let fuelCosts = input |> Seq.map (fun x -> if (x / 3 - 2) < 0 then 0 else x / 3 - 2)
    let fuelSum = fuelCosts |> Seq.sum
    if fuelSum = 0 then
        sum
    else
        calcTotalFuelRequirement (sum + fuelSum) fuelCosts
    

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = 
        parsed
        |> Seq.map (fun x -> x / 3 - 2)
        |> Seq.sum

    let part2 = calcTotalFuelRequirement 0 parsed

    part1.ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2019" "1" |> Async.RunSynchronously)
    Assert.Equal("3442987", part1)
    Assert.Equal("5161601", part2)