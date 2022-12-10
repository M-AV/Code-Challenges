module _2022_10

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Find the register value * cycle during the 20th, 60th, 100th, 140th, 180th and 220th cycle
// Task 2: Based on the described rules, print result to screen and see what it says (read problem https://adventofcode.com/2022/day/10#part2)

let parseInput (input : string seq) = 
    input

// Here we just calculate all signal strengths and put them in an array. In the end we can just take the indexes we are interested in
let part1 input =
    let mutable x = 1
    let mutable cycle = 1
    let signalStrengths = Array.zeroCreate 400
    let increaseCycle () =
        signalStrengths[cycle] <- x * cycle
        cycle <- cycle + 1

    input |> Seq.iter (fun cmd -> 
        match cmd with
        | "noop" -> 
            increaseCycle()
        | Prefix "addx " rest ->
            let number = int rest
            increaseCycle()
            increaseCycle()
            x <- x + number)


    signalStrengths[20] + signalStrengths[60] + signalStrengths[100] + signalStrengths[140] + signalStrengths[180] + signalStrengths[220]

// Same as before, except we print the right char instead of calculating signal strength
let part2 input =
    let mutable x = 1
    let mutable cycle = 0
    let increaseCycle () =
        cycle <- cycle + 1
        let idx = (cycle-1) % 40
        if (idx = 0) then
            printfn ""
        if (x-1 <= idx && idx <= x+1) then
            Console.ForegroundColor <- ConsoleColor.Green
            printf "#"
        else 
            Console.ForegroundColor <- ConsoleColor.DarkGray
            printf "."


    printfn "\nPart 2 solution: "
    input |> Seq.iter (fun cmd ->
        match cmd with
        | "noop" -> 
            increaseCycle()
        | Prefix "addx " rest ->
            let number = int rest
            increaseCycle()
            increaseCycle()
            x <- x + number
        )        
    Console.ResetColor()
    printfn ""
    printfn ""

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 input

    part2 input

    part1.ToString(), "See printout when executing"

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "10" |> Async.RunSynchronously)
    Assert.Equal("15220", part1)
    Assert.Equal("See printout when executing", part2)