module _2023_02

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Given a set of random draws, sum all ids of games that are possible with 12 red, 13 greens and 14 blues 
// Task 2: Find least number of cubes possible for each game. Multiply them per game and sum them all up.

type CubeCount = int * string

type Draw = CubeCount[]

let parseInput (input : string seq) = 
    let parseSet (set : string array) : Draw =
        set 
        |> Array.map (fun x -> x.Split ' ')
        |> Array.map (fun x -> CubeCount (int (x[0]), x[1]))

    input 
    |> Seq.map(fun x -> x.Split ": ")
    |> Seq.map(fun x -> (x[0].Split ' ')[1] , x[1])
    |> Seq.map(fun (x,y) -> int x, y.Split "; ")
    |> Seq.map(fun (id,y) -> id, y |> Array.map (fun x -> x.Split ", "))
    |> Seq.map(fun (id, y) -> id, y |> Array.map parseSet)
    |> List.ofSeq


let isPossible (count:int) (color:string) (set:Draw) : bool =
    set 
    |> Array.filter (fun (_, c) -> color = c) 
    |> Array.filter (fun (cnt, _) -> cnt > count)
    |> Array.isEmpty

let part1 (input:(int*Draw array) list) =
    let red set = not (isPossible 12 "red" set)
    let green set = not (isPossible 13 "green" set)
    let blue set = not (isPossible 14 "blue" set)
   
    let res = 
        input 
        |> List.filter (fun (id, sets) -> sets |> Array.filter red |> Array.isEmpty)
        |> List.filter (fun (id, sets) -> sets |> Array.filter green |> Array.isEmpty)
        |> List.filter (fun (id, sets) -> sets |> Array.filter blue |> Array.isEmpty)

    res |> List.map fst |> List.sum
    

let part2 (input:(int*Draw array) list) =
    let cubesets = input |> List.map snd

    // I kinda regretted my model choice here.. 

    /// Convert draw into (r,g,b) values instead
    let rec calcDraw (r,g,b) (game:CubeCount list) =
        match game with
        | []  -> (r, g, b)
        | draw::rest -> 
            let value = fst draw
            let res = 
                match snd draw with
                | "red" -> max value r, g, b
                | "green" -> r, max value g, b
                | "blue" -> r, g, max value b
            calcDraw res rest
    /// Take max value of each color across all draws of a game
    let calcGame (game: Draw array) = 
        game
        |> Array.map List.ofArray
        |> Array.map (calcDraw (0,0,0))
        |> Array.fold (fun (r,g,b) (ri, gi, bi) -> max r ri, max g gi, max b bi) (0,0,0)


    cubesets 
    |> List.map calcGame 
    |> List.map (fun (r,g,b) -> r * g * b) 
    |> List.sum

let execute (input : string seq) =
    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()


[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "2" |> Async.RunSynchronously)
    Assert.Equal("2683", part1)
    Assert.Equal("49710", part2)