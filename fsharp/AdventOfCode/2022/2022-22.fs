module _2022_22

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Start in top left corner of visible grid. Move along with the instructions, figure out where you end up 
//         assuming you wrap around when exiting the end. Answer = 1000*(y+1) + 4*(x+1) + direction.
// Task 2: Turns out the grid forms a cube. Do Part 1 except you wrap around the cube when going past edges. 

type Dir =
  | Forward of int
  | Turn of char

// 1. Parse grid into a 2D array where we include all the empty spaces
// 2. Iterate through directions and create a list of parsed integers and letters
let parseInput (input : string seq) = 
    let map = 
        input 
        |> Seq.takeWhile (fun s -> s.Length <> 0) 
        |> List.ofSeq
    let directions = 
        input 
        |> Seq.skip (map.Length+1) 
        |> Seq.take 1
        |> Seq.map (fun x -> 
            let mutable res = []
            let mutable i = 0

            while i < x.Length do
                let mutable idx = x.IndexOfAny([| 'R'; 'L' |], i+1)
                idx <- if idx < 0 then x.Length else idx
                res <- Forward(int x[i..idx-1])::res
                if idx < x.Length then
                    res <- Turn(x[idx])::res
                i <- idx+1

            res <- res |> List.rev
            res
            )
        |> Seq.exactlyOne

    let longestX = (map |> Seq.sortByDescending (fun x -> x.Length) |> Seq.take 1 |> Seq.exactlyOne).Length
    let mapArr = parseArray2d (map |> List.map (fun x -> x.PadRight longestX))

    mapArr, directions

// Go 1 step at a time and check if we're blocked before proceeding
let goForward_part1 (x,y) count direction map =
    let width = Array2D.length1 map
    let height = Array2D.length2 map

    // Function with all cases for directions. We check if we are at an edge and if we are we find the correct 
    // place to go by searching the right column/row.
    let func = 
        match direction with
        | '^' -> fun (x,y) -> 
            if y = 0 || map[x, y-1] = ' ' then
                let newY = seq { height-1 .. -1 .. 0 } |> Seq.find (fun y' -> map[x, y'] <> ' ')
                (x, newY)
            else
                (x, y-1)
        | '<' -> fun (x,y) -> 
            if x = 0 || map[x-1, y] = ' ' then
                let newX = seq { width-1 .. -1 .. 0 } |> Seq.find (fun x' -> map[x', y] <> ' ')
                (newX, y)
            else
                (x-1, y)
        | '>' -> fun (x,y) -> 
            if x = (width-1) || map[x+1, y] = ' ' then
                let newX = seq { 0 .. width-1 } |> Seq.find (fun x' -> map[x', y] <> ' ')
                (newX, y)
            else
                (x+1, y)
        | 'v' -> fun (x,y) -> 
            if y = (height-1) || map[x, y+1] = ' ' then
                let newY = seq { 0 .. height-1 } |> Seq.find (fun y' -> map[x, y'] <> ' ')
                (x, newY)
            else
                (x, y+1)
                
    let mutable loc = (x,y)
    let mutable i = 0
    let mutable found = false
    // Go 1 step at a time until we get blocked or reach the destination
    while i < count && not found do
        i <- i + 1
        let (xN,yN) = func loc
        if map[xN, yN] = '#' then
            found <- true
        elif map[xN, yN] = '.' then
            loc <- (xN, yN)
        else
            printfn "Should not happen.."

    loc, direction
// Same as before, except we calculate the right index to go to every time we wrap. This is hardcoded for 
// the shape of my input and doesn't work for the example input.
// 
// Example of shape of input with letters matching the side they connect to. 
// The letters are also to the correct side of where they match when folded.
// E.g. The top 'C' is to the left and the bottom 'C' is at the top (of the side),
//      because going out through the top C on the left, would make you enter 
//      at the top of the side at the bottom.
//
//           C     G
//         |-----|-----|
//         |           |
//         |     |     |
//       B |       E   |F
//         |- - -|-----|
//         |     |
//         |    E|
//         |A    |
//         |     |
//   |-----|- - -|
//  B|   A |     |F
//   |           |
//   |     |     |
//   |      D    |
//   |- - -|-----|
//  C|    D|
//   |     |
//   |     |
//   |-----|
//    G
let goForward_part2 (x,y) count direction (map:char array2d) =
    // Function like in Part 1, but with all cases hardcoded. 
    // Commented letters indicate which side in the image above they are handling
    let func = fun dir -> 
        match dir with
        | '^' -> fun (x,y) -> 
            if (x < 50 && y = 100) then // A
                (50, 50+x), '>'
            elif (50 <= x && x < 100 && y = 0) then // C
                (0, 100 + x), '>'
            elif (100 <= x && x < 150 && y = 0) then // G
                (x - 100, 199), '^'
            else
                (x, y-1), dir
        | '<' -> fun (x,y) -> 
            if y < 50 && x = 50 then // B
                (0, 100+(49-y)), '>'
            elif 50 <= y && y < 100 && x = 50 then // A
                (y-50, 100), 'v'
            elif 100 <= y && y < 150 && x = 0 then // B
                (50, 49 - (y - 100)), '>'
            elif 150 <= y && x = 0 then // C
                (y-100, 0), 'v'
            else
                (x-1, y), dir
        | '>' -> fun (x,y) -> 
            if y < 50 && x = 149 then // F
                (99, 100 + (49 - y)), '<'
            elif 50 <= y && y < 100 && x = 99 then // E
                (y + 50, 49), '^'  
            elif 100 <= y && y < 150 && x = 99 then // F
                (149, 49 - (y - 100)), '<'
            elif 150 <= y && x = 49 then // D
                (y - 100, 149), '^'
            else
                (x+1, y), dir
                
        | 'v' -> fun (x,y) -> 
            if (x < 50 && y = 199) then // G
                (x + 100, 0), 'v'
            elif (50 <= x && x < 100 && y = 149) then // D
                (49, x + 100), '<' 
            elif (100 <= x && y = 49) then // E
                (99, x - 50), '<'
            else
                (x, y+1), dir
                
                
    let mutable loc = (x,y)
    let mutable newDir = direction
    let mutable i = 0
    let mutable found = false
    while i < count && not found do
        i <- i + 1
        let (xN,yN), dir = (func newDir) loc
        if map[xN, yN] = '#' then
            found <- true
        elif map[xN, yN] = '.' then
            loc <- (xN, yN)
            newDir <- dir
        else
            printfn "Should not happen.."

    loc, newDir

// Helper function for debugging. Prints the whole grid.
let printGrid2d grid (steps:Map<int*int, char>) loc =
    for y = 0 to (Array2D.length2 grid)-1 do 
        let xs = steps.Keys |> Seq.map snd
        if (xs |> Seq.contains y) then
            for x = 0 to (Array2D.length1 grid)-1 do
                if (x,y) = loc then
                    Console.ForegroundColor <- ConsoleColor.Green
                    printf "%c" steps[(x,y)]
                    Console.ResetColor()
                elif (Map.containsKey (x,y) steps) then
                    Console.ForegroundColor <- ConsoleColor.Red
                    printf "%c" steps[(x,y)] 
                    Console.ResetColor()
                else
                    printf "%c" grid[x,y]
            printfn ""
        else 
            let s = String(grid[0..(Array2D.length1 grid)-1, y])
            printfn "%s" s
    printfn ""

// Simply run through each command and keep track of the right index and direction
let doYourMagic (map:char array2d) directions forward =
    let location = map[0 .. (map |> Array2D.length1)-1, 0] |> Array.findIndex (fun x -> x = '.')

    let turn direction turn =
        match turn, direction with
        | 'L', '^' -> '<'
        | 'L', '>' -> '^'
        | 'L', 'v' -> '>'
        | 'L', '<' -> 'v'
        | 'R', '^' -> '>'
        | 'R', '>' -> 'v'
        | 'R', 'v' -> '<'
        | 'R', '<' -> '^'

    let turnVal direction = 
        match direction with
        | '^' -> 3
        | '<' -> 2
        | 'v' -> 1
        | '>' -> 0

    let mutable steps = Map.empty
    let mutable enablePrint = false
    let rec solve currentLoc direction remaining =
        match remaining with
        | [] -> currentLoc, direction
        | head::tail ->
            match head with
            | Forward x -> 
                let newLoc, newDir = forward currentLoc x direction map
                solve newLoc newDir tail
            | Turn x ->
                let newDir = turn direction x
                steps <- steps |> Map.add currentLoc newDir
                // printGrid2d map steps currentLoc
                
                solve currentLoc newDir tail

    //printfn "Startloc: %A" location
    let loc,dir = solve (location, 0) '>' directions

    //printfn "EndLoc: %A %A" loc dir
    (snd loc + 1) * 1000 + (fst loc + 1) * 4 + (turnVal dir)


let execute (input : string seq) =
    let (map, directions) = parseInput input

    let part1 = doYourMagic map directions goForward_part1

    let part2 = doYourMagic map directions goForward_part2

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "22" |> Async.RunSynchronously)
    Assert.Equal("65368", part1)
    Assert.Equal("156166", part2)