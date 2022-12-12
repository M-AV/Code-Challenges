module _2022_12

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Find shortest path to destination
// Task 2: Find the shortest path from the destination to any 'a'

let parseInput (input : string seq) : int array2d * (int * int) * (int * int) = 
    let grid = input |> parseArray2d
    
    let startingPos = (grid |> find2d 'S').Value
    let targetPos = (grid |> find2d 'E').Value
    grid[fst startingPos, snd startingPos] <- 'a'
    grid[fst targetPos, snd targetPos] <- 'z'

    let res = grid |> Array2D.map int

    (res, startingPos, targetPos)

let availableDests grid (x,y) =
    [ (x-1,y); (x+1, y); (x, y-1); (x, y+1) ]
        |> List.filter (fun (x1,y1) -> x1 >= 0 && y1 >= 0)
        |> List.filter (fun (x1,y1) -> x1 < Array2D.length1 grid && y1 < Array2D.length2 grid)
       
// I fell into the trap thinking that we can only go 1 down instead of an infinite length down, 
// so I spent waaaay too long debugging why I didn't reach the destination
let print (distanceGrid:int array2d) grid =
    for y = 0 to (Array2D.length2 grid)-1 do 
        for x = 0 to (Array2D.length1 grid)-1 do
        
            if distanceGrid[x,y] = Int32.MaxValue then
                Console.ForegroundColor <- ConsoleColor.Red
                printf "%c" (char grid[x,y])
            else
                Console.ForegroundColor <- ConsoleColor.Green
                printf "%c" (char grid[x,y])
        printfn ""
    Console.ResetColor()

let part1 (grid : int array2d, (xS, yS), (xE, yE)) =
    let toReview = [ (xS, yS) ]
    let distanceGrid = Array2D.create (Array2D.length1 grid) (Array2D.length2 (grid)) Int32.MaxValue
    distanceGrid[xS, yS] <- 0

    let rec search (toEval:(int*int) list) =
        match toEval with
        | [] -> 
            //print distanceGrid grid
            distanceGrid[xE, yE]
               
        | (x,y)::tail ->
            let destsasdf = 
                availableDests grid (x,y) 
                |> List.filter (fun (x1,y1) -> (grid[x1,y1] - grid[x, y]) <= 1)
            let dests = 
                destsasdf 
                |> List.filter (fun (x',y') -> (distanceGrid[x, y]+1) < distanceGrid[x', y'])
            for (xD, yD) in dests do
                distanceGrid[xD, yD] <- min distanceGrid[xD, yD] (distanceGrid[x, y]+1)
            
            let updated = dests@tail

            search updated

    search toReview

// Reverse search so we start from E. Using BFS until we reach the first 'a'. 
// Then we have the shortest distance.
let part2 (grid:int array2d, _, (x,y)) =
    let toReview = [ (x, y) ]
    let distanceGrid = Array2D.create (Array2D.length1 grid) (Array2D.length2 (grid)) Int32.MaxValue
    distanceGrid[x, y] <- 0

    let rec search (toEval:(int * int) list) =
        match toEval with
        | [] -> 
            distanceGrid[x, y]
        | (x,y)::tail ->
            let dests = 
                availableDests grid (x,y) 
                |> List.filter (fun (x1,y1) -> (grid[x,y] - grid[x1, y1]) <= 1)
                |> List.filter (fun (x',y') -> (distanceGrid[x, y]+1) < distanceGrid[x', y'])

            for (xD, yD) in dests do
                distanceGrid[xD, yD] <- min distanceGrid[xD, yD] (distanceGrid[x, y]+1)
            
            match (dests |> List.tryFind (fun (x',y') -> grid[x',y'] = (int 'a'))) with
            | None -> 
                let updated = tail@dests
                search updated
            | Some (x',y') -> distanceGrid[x', y'];
            
    search toReview


let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "12" |> Async.RunSynchronously)
    Assert.Equal("423", part1)
    Assert.Equal("416", part2)