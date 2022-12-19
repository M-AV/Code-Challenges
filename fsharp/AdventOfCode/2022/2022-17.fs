module _2022_17

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Simulate Tetris moves for 2022 bricks and figure out how tall the tower is (no lines dissapear here)
// Task 2: Do it again, but for 1.000.000.000.000 bricks..

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
// Start from the top and follow all empty squares until we can't get down any further.
// Anything below this point is irrelevant for the new blocks coming in
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

// Prints out grid nicely for debugging purposes.
let printGrid grid block (xb,yb)=
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

// Verify that we can move block to this new position
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

// Put block permanently into the grid
let settle (grid: char array2d) brick (x,y) =
    let d = brick |> Array2D.iteri (fun xb yb c ->
        if (c = '#') then
            grid[x+xb, y+yb] <- '#'
        )

    y + (Array2D.length2 brick)
    
// Take 5 rows and compare them to all other rows to see how often those five a duplicated
let findFullRows grid lowestRow =
    let rowsToSearch = 5

    let maxX =(Array2D.length1 grid)-1
    let maxY = (Array2D.length2 grid)-1
    let compare = grid[0..maxX, 1..rowsToSearch]

    //printGrid2d compare
    let mutable matches = []
    for y = (rowsToSearch+1) to (maxY - rowsToSearch) do
        let toCompare = grid[0..maxX, y..(y+rowsToSearch)-1]
        let mutable isMatch = true
        for y' = 0 to (rowsToSearch-1) do
            for x' = 0 to maxX do
                if (compare[x',y'] <> toCompare[x',y']) then
                    isMatch <- false

        if (isMatch) then
            matches <- y::matches

    matches




// IDEA
// Simulate blocks as they are
// Repeatedly what the lowest achievable position is
// 'Forget' everything below that point
let part1 (input:string) (iterations:Int64) =
    let mutable grid = Array2D.init 7 5000 (fun x y -> if y = 0 then '#' else '.')
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

        // Here we find the lowest possible place a brick could go and remove everything else to keep the array small
        // This was made in anticipation of part 2 where I thought it would just be the same task but some much higher number.. 
        // Turns out I was right... Except it was so high that even with this it doesn't matter as the runtime to simulate everything
        // is simply too high. -.-
        if (i % 500L = 0) then
            let lowestPoint = (findLowestReachablePoint grid gridHeight) - 1
            let newGrid = Array2D.init 7 5000 (fun x y -> 
                if (y < (gridHeight - lowestPoint)) then
                    grid[x, y+lowestPoint]
                else 
                    '.'
                )
            grid <- newGrid
            gridHeight <- gridHeight - lowestPoint

        brickIdx <- (brickIdx+1) % 5

    currentHeight - 1L

// 1. Run simulation until we have detected a cycle 
// 2. Find start and end of that cycle, so we can calculate:
//       - How many bricks in a cycle
//       - How long a cycle is
//       - Where the first cycle starts
// 3. Calculate how many cycles in the 1.000.000.000.000 bricks and how high that would be
// 4. Use part 1 to figure out how high the remaining bricks are
//
// I didn't know of a good way of finding the cycles, so I kinda brute force it which takes some time.
// Both parts take around 2-3 minutes on my machine.
let part2 (input:string) =
    let mutable grid = Array2D.init 7 50000 (fun x y -> if y = 0 then '#' else '.')
    let mutable currentHeight = 1L
    let mutable gridHeight = 1

    let mutable brickIdx = 0
    let mutable windIdx = 0

    let mutable i = 1L

    let heightMap = Array.zeroCreate 50000
    let mutable brickCnt = 0

    let mutable startIt = 0L
    let mutable endIt = 0L

    let mutable foundTwo = false
    let mutable heightOne = 0L
    let mutable heightTwo = 0L

    while (not foundTwo) do
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
                    heightMap[brickCnt] <- currentHeight
                    
                    brickCnt <- brickCnt+1

            windIdx <- (windIdx + 1) % input.Length


        // I don't know the best way to idenfify duplicates, so now we just search every time we add one until we have seen the right dupes
        if (i > 100 && not foundTwo) then
            let fullRows = findFullRows grid gridHeight
            if endIt = 0 then
                if (fullRows.Length = 2 && startIt = 0) then
                    startIt <- i-1L
                    heightOne <- currentHeight
                if (fullRows.Length = 3 && endIt = 0) then
                    endIt <- i-1L
                    foundTwo <- true
                    heightTwo <- currentHeight

        brickIdx <- (brickIdx+1) % 5


    //printfn "Brick start: %i" startIt
    //printfn "  Brick end: %i" endIt
    //printfn "Heigh start: %i" heightOne
    //printfn " Height end: %i" heightTwo

    let totalIterations = 1000000000000L
    let bricksPerIteration = endIt - startIt
    let heightPerIteration = heightTwo - heightOne
    let bricksUntilStartOfFirstIteration = startIt
    let bricksInIterations = totalIterations - bricksUntilStartOfFirstIteration
    let iterations = bricksInIterations / bricksPerIteration
    let remainingAfterIterations = bricksInIterations % bricksPerIteration
    let remainingHeight = (part1 input (remainingAfterIterations + startIt)) - (startIt-1L)

    let res = (iterations * heightPerIteration) + (startIt - 1L) + remainingHeight

    res

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let p1 = part1 parsed 2022
    let part2 = part2 parsed

    p1.ToString(), part2.ToString()

//[<Fact>] // Takes a couple of minutes
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "17" |> Async.RunSynchronously)
    Assert.Equal("3215", part1)
    Assert.Equal("1575811208171", part2)