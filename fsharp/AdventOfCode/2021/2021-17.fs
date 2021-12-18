module _2021_17

open System
open Xunit

// Task 1: Find out how high you can shoot probe and still hit target
// Task 2: Count all possible shots we can take

type TargetArea = (int * int) * (int * int)

let charToInt (c:char) = (int c) - (int '0')

let parseInput (input : string seq) =
    let parsed = 
        input 
        |> Seq.map(fun x -> x.Split("target area: x=", StringSplitOptions.RemoveEmptyEntries)[0])
        |> Seq.map (fun x -> x.Split(", y="))
        |> Seq.head 
        |> Seq.map (fun x -> x.Split("..") |> Array.map (fun v -> int v))
        |> Seq.map (fun x -> (x[0], x[1]))
        |> Array.ofSeq
    TargetArea(parsed[0], parsed[1])

let isWithinTarget point input =
    let (x, y) = point
    fst(fst input) <= x && x <= snd(fst input) && fst(snd input) <= y && y <= snd(snd input)

let rec simulateShot xVel yVel point yMax input=
    match point with
    | (x, _) when x < fst(fst input) && xVel = 0 -> (false, -1)
    | (x, _) when snd(fst input) < x -> (false, -1)
    | (_, y) when y < fst(snd input) -> (false, -1)
    | (x, y) when isWithinTarget (x,y) input -> (true, yMax)
    | (x, y) -> 
        let newX = x + xVel
        let newY = y + yVel
        let newXVel = if xVel > 0 then xVel - 1 else if xVel < 0 then xVel + 1 else 0
        let newYVel = yVel - 1
        simulateShot newXVel newYVel (newX, newY) (max yMax newY) input

let rec findMinXVel xVel input =
    if ((xVel * (xVel + 1)) / 2) < fst(fst input) then
        findMinXVel (xVel + 1) input
    else
        xVel
let rec findMaxXVel xVel input =
    if ((xVel * (xVel + 1)) / 2) <= snd(fst input) then
        findMaxXVel (xVel + 1) input
    else
        xVel - 1

let highestPossibleShot (input:TargetArea) =
    let minXVel = (findMinXVel 0 input) - 1
    let maxXVel = findMaxXVel 0 input
    // If we shoot up, we will always hit 0. The step after will have y velocity -(original y) - 1. So we have a cap on our y velocity
    let maxYVel = fst(snd input) * -1 
    let mutable maxY = 0
    for xVel in minXVel .. maxXVel do
        for yVel in 1 .. maxYVel do
            match simulateShot xVel yVel (0,0) 0 input with
            | (true, shotMaxY) ->
                maxY <- max maxY shotMaxY
            | _ -> maxY <- maxY
    maxY
let allPossibleShots input =
    // It doesn't take super long to iterate from 0, so we're just doing that instead of calculating a "minimum x velocity"
    let minXVel = 1
    let maxXVel = snd (fst input)
    // If we shoot down, we can at most shoot directly to the right depth, anything more will overshoot and we'll never shoot backwards
    let minYVel = fst(snd input)
    let maxYVel = fst(snd input) * -1 

    let mutable paths = 0
    for xVel in minXVel .. maxXVel do
        for yVel in minYVel .. maxYVel do
            match simulateShot xVel yVel (0,0) 0 input with
            | (true, _) ->
                paths <- paths + 1
            | _ -> paths <- paths
    paths

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = highestPossibleShot parsed

    let part2 = allPossibleShots parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Example 1``() =
    let input = [ "target area: x=20..30, y=-10..-5" ]
    let (part1, part2) = execute input
    Assert.Equal("45", part1)
    Assert.Equal("112", part2)

[<Fact>]
let ``Actual Input``() =
    let input = [ "target area: x=144..178, y=-100..-76" ]
    let (part1, part2) = execute input
    Assert.Equal("4950", part1)
    Assert.Equal("1477", part2)