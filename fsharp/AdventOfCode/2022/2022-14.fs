module _2022_14

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Using description of cave system and sand physics, figure out how many dropping 'sand blocks' can fit
// Task 2: Add floor and then see how many can fit

let print grid =
    let mutable mutableMaxX = -1
    let mutable mutableMinX = Int32.MaxValue
    let mutable maxY = -1;

    grid |> Array2D.iteri (fun x y v -> 
        if (v <> '.') then
            mutableMaxX <- (max x mutableMaxX)
            mutableMinX <- (min x mutableMinX)
            maxY <- (max y maxY)
        )

    for y = 0 to (min (maxY+1) ((Array2D.length2 grid)-1)) do 
        for x = (max (mutableMinX-1) 0) to (min (mutableMaxX+1) ((Array2D.length1 grid)-1)) do
            if (x = 500 && y = 0) then
                printf "%c" '+'
            else
                printf "%c" grid[x,y]
        printfn ""
    printfn ""

let findMaxY grid =
    let mutable mutableMaxX = -1
    let mutable mutableMinX = Int32.MaxValue
    let mutable maxY = -1;

    grid |> Array2D.iteri (fun x y v -> 
        if (v = '#') then
            mutableMaxX <- (max x mutableMaxX)
            mutableMinX <- (min x mutableMinX)
            maxY <- (max y maxY)
        )
    maxY

let parseInput (input : string seq) = 
    let parsedLocs = 
        input 
        |> Seq.map (fun line -> (line.Split " -> ") |> Array.map (fun loc -> loc.Split ","))
        |> Seq.map (fun x -> x |> Array.map (fun loc -> int loc[0], int loc[1])) |> List.ofSeq

    let gridSize = 
        parsedLocs 
        |> Seq.map (fun x -> x |> Array.map (fun (l,r) -> max l r)) |> Seq.map Array.max |> Seq.max

    let grid = Array2D.init (gridSize+500) (gridSize+3) (fun x y -> '.') // Lets give enough room to the side
    let mutable maxY = 0
    parsedLocs |> List.iter (fun line -> 
        line
        |> Array.pairwise 
        |> Array.iter (fun ((fstX, fstY),(sndX, sndY)) -> 
            if (fstX = sndX) then
                for y = (min fstY sndY) to (max fstY sndY) do
                    grid[fstX,y] <- '#'
                    maxY <- max maxY y
            else 
                for x = (min fstX sndX) to (max fstX sndX) do 
                    grid[x,fstY] <- '#'
            ))

    grid[0.., 0..(maxY+1)]
 
// Here we simply keep simulating sand-drops until we fall out of bounds in the bottom
let part1 (input:char array2d) =
    let rec simulateSandDrop (x,y) =
        if (y >= (Array2D.length2 input)-1) then
            true
        elif (input[x, y+1]='.') then
            simulateSandDrop (x,y+1)
        elif (input[x-1, y+1]='.') then
            simulateSandDrop (x-1, y+1)
        elif (input[x+1, y+1]='.') then
            simulateSandDrop (x+1, y+1)
        else
            input[x,y] <- 'O'
            //print input
            false


    let mutable counted = 0
    let mutable found = false
    while (not found) do
        found <- simulateSandDrop (500,0)
        counted <- counted + 1


    counted-1

// Same as before, but consider the bottom solid. When parsing we make the array big enough to 
// not overflow to the sides as that seemed the easiest solution
let part2 (input:char array2d) =
    let rec simulateSandDrop (x,y) =
        if (input[500,0] = 'O') then
            true
        elif ((y+1) = (Array2D.length2 input)) then
            input[x,y] <- 'O'
            false
        elif (input[x, y+1]='.') then
            simulateSandDrop (x,y+1)
        elif (input[x-1, y+1]='.') then
            simulateSandDrop (x-1, y+1)
        elif (input[x+1, y+1]='.') then
            simulateSandDrop (x+1, y+1)
        else
            input[x,y] <- 'O'
            //print input
            false


    let mutable counted = 0
    let mutable found = false
    while (not found) do
        found <- simulateSandDrop (500,0)
        counted <- counted + 1


    counted-1

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    print parsed
    //printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 (parseInput input)

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "14" |> Async.RunSynchronously)
    Assert.Equal("832", part1)
    Assert.Equal("27601", part2)