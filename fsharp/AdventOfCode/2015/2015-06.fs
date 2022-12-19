module _2015_6

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Text.RegularExpressions

// Task 1: Toggle bits in a 1000x1000 grid in the right order and figure out how many are lit up in the end
// Task 2: On now means increase by 1, off decrease by one and toggle increase by 2

// Parse input using Regex that captures each number and find action with a simple string.Contains
let parseInput (input : string seq) = 
    let rangeRegex = Regex("(\d+),(\d+) .* (\d+),(\d+)", RegexOptions.Compiled)
    
    input 
    |> Seq.map (fun x -> 
        (if x.Contains "on" then 1 elif x.Contains "off" then -1 else 0), x)
    |> Seq.map (fun (x,y) -> 
        let matches = rangeRegex.Matches(y)
        let xS = int (matches[0].Groups[1].Value)
        let yS = int (matches[0].Groups[2].Value)
        let xE = int (matches[0].Groups[3].Value)
        let yE = int (matches[0].Groups[4].Value)
        
        x, (xS,yS), (xE,yE)
            )
    |> List.ofSeq

// Just iterate through everything and perform the right action
let part1 input =
    let grid = Array2D.init 1000 1000 (fun x y -> false)

    input
    |> List.iter (fun (action, start, stop) ->
        let newVal = if action = -1 then (fun x -> false) elif action = 1 then (fun x -> true) else (fun x -> not x) 

        for x = fst start to fst stop do
            for y = snd start to snd stop do 
                grid[x,y] <- newVal grid[x,y]
    )

    let mutable count = 0
    for x = 0 to 999 do
        for y = 0 to 999 do
            if (grid[x,y]) then
                count <- count + 1
    count

// Same same, just different functions
let part2 input =
    let grid = Array2D.zeroCreate 1000 1000

    input
    |> List.iter (fun (action, start, stop) ->
        let newVal = if action = -1 then (fun x -> max (x - 1) 0) elif action = 1 then (fun x -> x + 1) else (fun x -> x + 2) 

        for x = fst start to fst stop do
            for y = snd start to snd stop do 
                grid[x,y] <- newVal grid[x,y]
    )

    let mutable count = 0
    for x = 0 to 999 do
        for y = 0 to 999 do
                count <- grid[x,y] + count
    count

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "6" |> Async.RunSynchronously)
    Assert.Equal("569999", part1)
    Assert.Equal("17836115", part2)