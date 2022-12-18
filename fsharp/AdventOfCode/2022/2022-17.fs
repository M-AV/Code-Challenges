module _2022_17

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: 
// Task 2: 

let parseInput (input : string seq) = 
    input |> Seq.exactlyOne

// Mirrored so they fit us going down instead of up
let bricks = [|
    array2D [
        ['#'];['#'];['#'];['#'];
    ];
    array2D [|
        ['.';'#';'.'];
        ['#';'#';'#'];
        ['.';'#';'.'];
    |];
    array2D [|
        ['#';'.';'.'];
        ['#';'.';'.'];
        ['#';'#';'#'];
    |];
    array2D [|
        ['#';
         '#';
         '#';
         '#'];
    |];
    array2D [|
        ['#';'#'];
        ['#';'#'];
    |]
|]

let findLowestReachablePoint (grid:char array2d) currentHeight =
    let startPos = 2, currentHeight

    let mutable lowestNode = currentHeight

    let mutable visitedNodes = Set.empty
    let mutable toVisit = [startPos]

    while (not toVisit.IsEmpty) do
        let (x,y) = toVisit.Head
        toVisit <- toVisit.Tail
        visitedNodes <- visitedNodes.Add((x,y))

        if (grid[x,y] <> '#') then
            if (lowestNode > y) then
                lowestNode <- y
            if (x > 0 && not (visitedNodes.Contains (x-1, y))) then
                toVisit <- (x-1,y)::toVisit
            if (x < 6 && not (visitedNodes.Contains (x+1, y))) then
                toVisit <- (x+1,y)::toVisit
            if (y > 0 && not (visitedNodes.Contains (x, y-1))) then
                toVisit <- (x, y-1)::toVisit

    lowestNode

let printGrid grid block (xb,yb)=
    let mutable mutableMaxX = -1
    let mutable mutableMinX = Int32.MaxValue
    let mutable maxY = -1;
    for y = 0 to (Array2D.length2 grid)-1 do 
        for x = 0 to (Array2D.length1 grid)-1 do
            let blockHeight = Array2D.length2 block
            let blockWidth = Array2D.length1 block
            if ((y >= yb && y <= (blockHeight - 1) + yb) && (x >= xb && x <= (xb + blockWidth) - 1)) then
                Console.ForegroundColor <- ConsoleColor.Red
                printf "%c" block[(x-xb), y-yb]
            else
                Console.ResetColor()
                printf "%c" grid[x,y]
        printfn ""
    printfn ""

// IDEA
// Simulate blocks as they are
// Repeatedly what the lowest achievable position is
// 'Forget' everything below that point
let part1 (input:string) (iterations:Int64) =
    let verify (grid:char array2d) brick (x,y) =
        if (x < 0 || ((Array2D.length1 brick) + x) > 7) then
            false
        else
            let mutable isValid = true
            brick |> Array2D.iteri (fun xb yb c ->
                if (c = '#') then
                    if (grid[x+xb, y+yb] = '#') then
                        isValid <- false   
                )
            isValid

    let settle (grid: char array2d) brick (x,y) =
        let d = brick |> Array2D.iteri (fun xb yb c ->
            if (c = '#') then
                grid[x+xb, y+yb] <- '#'
            )

        y + (Array2D.length2 brick)
    

    let mutable grid = Array2D.init 7 100000 (fun x y -> if y = 0 then '#' else '.')
    let mutable currentHeight = 1L
    let mutable gridHeight = 1

    let mutable brickIdx = 0
    let mutable windIdx = 0

    let mutable i = 1L

    while (i <= iterations) do
        i <- i + 1L;
        let mutable settled = false
        let brick = bricks[brickIdx]

        let mutable pos = 2, gridHeight + 3
        while (not settled) do
            // Simulate wind
            let mutable newPos = match input[windIdx] with
                | '<' -> (fst pos)-1, snd pos
                | '>' -> (fst pos)+1, snd pos
                
            if (verify grid brick newPos) then
                pos <- newPos

            // Simulate 1 gravity
            newPos <- (fst pos), (snd pos) - 1
            if (verify grid brick newPos) then
                pos <- newPos
            else
                settled <- true
                let newHeight = settle grid brick pos
                if (newHeight > gridHeight) then
                    currentHeight <- currentHeight + ((int64 newHeight) - (int64 gridHeight))
                    gridHeight <- newHeight

            windIdx <- (windIdx + 1) % input.Length
        
        if (i % 50000L = 0) then
            let lowestPoint = (findLowestReachablePoint grid gridHeight) - 1
            let newGrid = Array2D.init 7 100000 (fun x y -> 
                if (y < (gridHeight - lowestPoint)) then
                    grid[x, y+lowestPoint]
                else 
                    '.'
                )
            grid <- newGrid
            gridHeight <- gridHeight - lowestPoint

        if (i % 1000000000L = 0) then
            printfn "%i billion down" (i / 1000000000L)

        brickIdx <- (brickIdx+1) % 5

    currentHeight - 1L


let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let p1 = part1 parsed 2022

    let part2 = "N/A"

    p1.ToString(), part2.ToString()

//[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "17" |> Async.RunSynchronously)
    Assert.Equal("N/A", part1)
    Assert.Equal("N/A", part2)