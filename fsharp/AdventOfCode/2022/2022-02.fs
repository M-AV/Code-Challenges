module _2022_2

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Calculate score according to listet rules
// Task 2: Calculate score according to updated rules (definition of 2nd column of letters have changed to the expected outcome)

let parseInput (input : string seq) : (char * char) list= 
    input |> Seq.map (fun s -> (s[0], s[2])) |> List.ofSeq

// According to rules from task 1
// // X = Rock, Y = Paper, Z = Scissor
let evaluateGamePart1 game =
    let selectionPoints = 
       match (snd game) with
       | 'X' -> 1
       | 'Y' -> 2
       | 'Z' -> 3

    let matchPoints =
        match game with 
        | ('A', 'X') | ('B', 'Y') | ('C', 'Z') -> 3 // Ties
        | ('A', 'Z') | ('B', 'X') | ('C', 'Y') -> 0 // Losses
        | ('A', 'Y') | ('B', 'Z') | ('C', 'X') -> 6 // Wins

    selectionPoints + matchPoints

// X = Loss, Y = Draw, Z = Win
let evaluateGamePart2 (game:char * char)=
    match game with 
    | ('A', 'X') -> 0 + 3
    | ('B', 'Y') -> 3 + 2
    | ('C', 'Z') -> 6 + 1
    | ('A', 'Z') -> 6 + 2
    | ('B', 'X') -> 0 + 1
    | ('C', 'Y') -> 3 + 3
    | ('A', 'Y') -> 3 + 1
    | ('B', 'Z') -> 6 + 3
    | ('C', 'X') -> 0 + 2

let calculatePart1 (input) =
    let rec calcScore currentScore remainingInput =
        match remainingInput with
        | head::tail -> calcScore (currentScore + (evaluateGamePart1 head)) tail
        | [] -> currentScore

    calcScore 0 input

let calculatePart2 input =
    input |> List.map (fun x -> evaluateGamePart2 x) |> List.sum
    

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = calculatePart1 parsed

    let part2 = calculatePart2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "2" |> Async.RunSynchronously)
    Assert.Equal("12645", part1)
    Assert.Equal("11756", part2)