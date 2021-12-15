module _2021_15

// Task 1: Find path lowest sum path through grid with only horizontal and vertical moves
// Task 2: 

let parseInput (input: string seq) =
    input 
    |> Seq.map (fun x -> x |> Seq.map (fun y -> (int64 y) - (int64 '0')))
    |> Seq.map Array.ofSeq
    |> Array.ofSeq

let findLowestRiskPath (grid:int64[][]) =
    let height = grid.Length
    let width = grid[0].Length

    let costs = Array2D.create (height) (width) 0L

    grid[0][0] <- 0
    for y in 0 .. height-1 do
        for x in 0 .. height-1 do
            match (x,y) with
            | (0,0) -> costs[x,y] <- 0
            | (0, y) -> costs[0,y] <- costs[0, y-1] + grid[x][y]
            | (x, 0) -> costs[x,0] <- costs[x-1, 0] + grid[x][y]
            | (x, y) -> costs[x,y] <-(min costs[x-1, y] costs[x, y-1]) + grid[x][y];

    //printfn "%A" costs
    costs[width-1,height-1]

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = findLowestRiskPath parsed

    let part2 = "N/A"

    part1.ToString(), part2.ToString()