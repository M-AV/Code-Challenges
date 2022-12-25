module _2022_24

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: You have to go from the top left to the bottom right. You can go up,right,down & left. 
//         Each angle bracket (blizzards) moves 1 in the direction they point on every turn. 
//         You have to avoid those.
//         What is the shortest time you can go to the destination?
// Task 2: Now go back and forth an extra time, how much time does that take?

// Parse the start and end positions. 
// Since blizzards can overlap, create an array of all of them with their direction and current position.
// To make finding possible moves easier, create a 2D grid with all walls and current blizzard positions.
// We also get height and width, though we could have gotten those from the grid later.
let parseInput (input : string seq) = 
    let grid = input |> parseArray2d
    let gridWidth = Array2D.length1 grid;
    let gridHeight = Array2D.length2 grid;

    let startPos = grid[0.., 0] |> Array.findIndex ((=) '.')
    let endPos = grid[0.., gridHeight - 1] |> Array.findIndex ((=) '.')

    let mutable positions = []
    grid |> Array2D.iteri (fun x y v -> 
        if (v <> '.' && v <> '#') then
            positions <- (v, (x,y))::positions
        )

    let blizzardMap = 
        grid |> Array2D.map (fun x ->
            if x = '<' || x = 'v' || x = '>' || x = '^' then
                1
            elif x = '#' then
                -1
            else 
                0
            )
    (startPos, 0), (endPos, gridHeight-1), (positions |> Array.ofList), blizzardMap, gridHeight, gridWidth

// Solves both parts in ~1.5 seconds
let blizzTime (startLoc, endLoc, (b: (char*(int*int)) array), map, gridHeight, gridWidth) =
    // To avoid sideeffects we create copies (not really needed as this function solves both parts)
    let blizzards = Array.copy b
    let blizMap = Array2D.copy map

    // Move all blizzards 1 step and update the 2D grid and the array. The wrap-around is a bit tricky as 
    // the start and end positions can also contain blizzards (at least I assume so)
    let iterateBlizzards () =
        blizzards 
        |> Array.iteri (fun i v -> 
            let f = 
                match v with
                | ('v', (x,y)) -> fun (x,y) -> x, (y+1) % (gridHeight)
                | ('<', (x,y)) -> fun (x,y) -> 
                    if x-1 < 0 then
                        gridWidth - 1, y
                    else
                        x-1, y
                | ('>', (x,y)) -> fun (x,y) -> (x+1) % (gridWidth), y
                | ('^', (x,y)) -> fun (x,y) -> 
                    if y-1 < 0 then
                        x, gridHeight - 1
                    else
                        x, y-1

            let mutable newLoc= f(snd v)
            while blizMap[fst newLoc, snd newLoc] < 0 do
                newLoc <- f(fst newLoc, snd newLoc)

            let (oldX, oldY) = snd v
            blizMap[oldX, oldY] <- blizMap[oldX, oldY]-1
            blizMap[fst newLoc, snd newLoc] <- blizMap[fst newLoc, snd newLoc]+1
            blizzards[i] <- fst v, (fst newLoc, snd newLoc)
        ) 
    // Find possible moves from the current position, assuming grid is updated
    let possibleMoves (x,y) =
        [(x-1, y); (x, y-1); (x+1, y); (x, y+1); (x,y)]
        |> List.filter (fun (xN,yN) -> xN >= 0 && yN >= 0)
        |> List.filter (fun (xN,yN) -> xN < gridWidth && yN < gridHeight)
        |> List.filter (fun (xN,yN) -> blizMap[xN, yN] = 0)

    // BFS keeping track of all possible positions for the current blizzard step
    let rec solve remPos (newPos:Set<int*int>) targetLoc stepCount =
        match remPos with
        | [] -> 
            iterateBlizzards()
            solve (newPos |> List.ofSeq) Set.empty targetLoc (stepCount+1)
        | head::tail ->
            if head = targetLoc then
                stepCount
            else
                let newPossible = possibleMoves head
                let allPos = newPos |> Set.union (newPossible |> Set.ofList)
                solve tail allPos targetLoc stepCount

    iterateBlizzards()
    let part1Res = solve [startLoc] Set.empty endLoc 0

    iterateBlizzards()
    let timeBack = solve [endLoc] Set.empty startLoc 0

    iterateBlizzards()
    let timeBackAgain = solve [startLoc] Set.empty endLoc 0

    part1Res, timeBack + 1, timeBackAgain + 1

let execute (input : string seq) =
    let parsed = parseInput input

    let (fst, snd, trd) = blizzTime parsed

    let part1 = fst

    let part2 = fst + snd + trd

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "24" |> Async.RunSynchronously)
    Assert.Equal("286", part1)
    Assert.Equal("820", part2)