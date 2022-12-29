module _2015_9

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Travelling Santa problem. Find shortest path to visit each city
// Task 2: Find longest

let parseInput (input : string seq) = 
    let distances = 
        input
        |> Seq.map (fun x -> 
            let parts = x.Split " = "
            let distance = int parts[1]
            let cities = parts[0].Split " to "
            [(cities[0], cities[1]), distance; (cities[1], cities[0]), distance]
            )
        |> Seq.collect (fun l -> l)
        |> Map.ofSeq
    let cities = distances.Keys |> Seq.map fst |> Set.ofSeq

    distances, cities

let part1 ((distances:Map<string*string, int>), cities) =
    let rec solve travelled currentCity (remaining:Set<string>) =
        if remaining.IsEmpty then
            travelled
        else
            remaining 
            |> Seq.map (fun x -> 
                let dist = distances[(currentCity, x)]
                solve (travelled + dist) x (remaining |> Set.remove x))
            |> Seq.min

    cities 
    |> Seq.map (fun x -> 
        solve 0 x (cities |> Set.remove x))
    |> Seq.min

// Instead of passing in the max/min function I just copied the code..
let part2 ((distances:Map<string*string, int>), cities) =
    let rec solve travelled currentCity (remaining:Set<string>) =
        if remaining.IsEmpty then
            travelled
        else
            remaining 
            |> Seq.map (fun x -> 
                let dist = distances[(currentCity, x)]
                solve (travelled + dist) x (remaining |> Set.remove x))
            |> Seq.max

    cities 
    |> Seq.map (fun x -> 
        solve 0 x (cities |> Set.remove x))
    |> Seq.max

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "9" |> Async.RunSynchronously)
    Assert.Equal("207", part1)
    Assert.Equal("804", part2)