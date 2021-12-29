module _2016_01

open Calculations
open InputProvider
open System
open Xunit

// Task 1: Find shortest distance to destination (manhattan distance)
// Task 2: Find shortest distance to first point we see twice 

type Direction =
    | North
    | South
    | East
    | West

let parseInput (input : string seq) = 
    input
    |> Seq.map (fun x -> x.Split ", ")
    |> Seq.head
    |> Seq.map (fun x -> (x[0], Int32.Parse x[1..]))
    |> List.ofSeq

let getDirection current direction =
    match (current, direction) with
    | (North, 'R') -> East
    | (North, 'L') -> West
    | (South, 'R') -> West
    | (South, 'L') -> East
    | (East, 'R') -> South
    | (East, 'L') -> North
    | (West, 'R') -> North
    | (West, 'L') -> South

let followDirection (current, x, y) (direction, dist) =
    match getDirection current direction with
    | North -> (North, x, y - dist)
    | South -> (South, x, y + dist)
    | East -> (East, x + dist, y)
    | West -> (West, x - dist, y)

let rec findLocationWeSeeTwice seenLocations (current, x, y) directions =
    match directions with
    | (turnRL, dist)::tail -> 
        let (direction, locs) =
            match getDirection current turnRL with
            | North -> (North, [y - 1 .. -1 .. y - dist] |> List.map (fun y -> (x, y)))
            | South -> (South, [y + 1 ..y + dist] |> List.map (fun y -> (x, y)))
            | East -> (East, [x + 1 .. x + dist] |> List.map (fun x -> (x, y)))
            | West -> (West, [x - 1 .. -1 .. x - dist] |> List.map (fun x -> (x, y)))

        let locationSeenTwice = locs |> List.tryFind (fun x -> Set.contains x seenLocations)

        let mutable set = seenLocations
        let mutable currentLoc = locs.Head
        match locationSeenTwice with
        | Some x -> x
        | None ->
            for loc in locs do
                set <- set.Add loc
                currentLoc <- loc
            findLocationWeSeeTwice set (direction, fst currentLoc, snd currentLoc) tail
    | [] -> (Int32.MaxValue, Int32.MaxValue)
    
let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let (direction, x, y) = parsed |> Seq.fold (fun agg cur -> followDirection agg cur) (North, 0, 0)
    let part1 = manhattanDistance2D (0,0) (x,y)

    let startSet = Set.empty.Add(0,0)
    let destinationSeenTwice = findLocationWeSeeTwice startSet (North, 0,0) parsed
    let part2 = manhattanDistance2D (0,0) destinationSeenTwice

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2016" "1" |> Async.RunSynchronously)
    Assert.Equal("253", part1)
    Assert.Equal("126", part2)