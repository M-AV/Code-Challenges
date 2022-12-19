module _2022_18

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Given a bunch of 3D coordinates for 1x1x1 cubes, count all sides not connected to another cube
// Task 2: The coordinates form a 'lava droplet' calculate the surface area (like before) but exclude any empty holes inside the droplet

let parseInput (input : string seq) = 
    input |> Seq.map (fun x -> x.Split ',') |> Seq.map (fun x -> int x[0], int x[1], int x[2]) |> List.ofSeq

let edgeCount (grid:int array3d) (x,y,z) =
    let neighbors = [(x-1, y, z); (x+1, y, z); (x, y-1,z); (x,y+1,z); (x,y,z-1); (x,y,z+1)]
    let res = neighbors |> Seq.filter (fun (xB,yB,zB) -> xB < 0 || yB < 0 || zB < 0 || grid[xB, yB, zB] <> 1) |> Seq.length
    res

// 1. Create grid 
// 2. For each filled square, count all sides that are not filled
let part1 input =
    let maxVal = (input |> Seq.map (fun (x,y,z) -> max (max x y) z) |> Seq.max) + 2
    let grid = Array3D.zeroCreate maxVal maxVal maxVal

    for (x,y,z) in input do
        grid[x,y,z] <- 1

    input |> Seq.map (fun x -> edgeCount grid x) |> Seq.sum

let canReachEdge grid loc =
    let mutable visited = Set.empty
    let mutable toVisit = [loc]
    let mutable foundEdge = false
    let gridSize = (Array3D.length1 grid) - 1

    while (not toVisit.IsEmpty) && not foundEdge do
        let (x,y,z) = toVisit.Head
        toVisit <- toVisit.Tail
        if (not (visited.Contains (x,y,z))) then
            visited <- visited.Add((x,y,z))

            if (x <= 0 || y <= 0 || z <= 0 || x >= gridSize || y >= gridSize || z >= gridSize) then
                foundEdge <- true
            else
                let emptyNeighbors = 
                    [(x-1, y, z); (x+1, y, z); (x, y-1,z); (x,y+1,z); (x,y,z-1); (x,y,z+1)] 
                    |> Seq.filter (fun (xN, yN, zN) -> grid[xN, yN, zN] = 0) 
                    |> Seq.filter (fun x -> not (visited.Contains x))
                    |> List.ofSeq
                toVisit <- emptyNeighbors @ toVisit

    foundEdge, visited

// 1. Create grid, 
// 2. Run a path analysis on each empty square to see if we can reach an edge, if we can't it must be a pocket and we fill it up
// 3. Run same calculation as in part 1 as we have no pockets anymore
let part2 input =
    let maxVal = (input |> Seq.map (fun (x,y,z) -> max (max x y) z) |> Seq.max) + 2
    let grid = Array3D.zeroCreate maxVal maxVal maxVal

    for (x,y,z) in input do
        grid[x,y,z] <- 1

    // Fill holes
    grid 
    |> Array3D.iteri (fun x y z v -> 
        if (not (v = 1) && x <> 0 && y <> 0 && z <> 0) then
            let isEdge, squares = canReachEdge grid (x,y,z)
            if (not (isEdge)) then
                squares |> Set.iter (fun (x', y', z') -> grid[x',y',z'] <- 1) // If the checked square is a pocket, all the empty squares next to it must also be a pocket
            )

    input |> Seq.map (fun x -> edgeCount grid x) |> Seq.sum
            
let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "18" |> Async.RunSynchronously)
    Assert.Equal("3346", part1)
    Assert.Equal("1980", part2)
