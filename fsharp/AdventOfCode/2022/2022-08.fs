module _2022_8

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Each number in grid represents a 'tree height'. Figure out how many tree can be seen if you only look from directly from a side (not diagonally)
// Task 2: Find largest possible 'scenic score' of a tree in the grid. Scenic score being how many trees can be seen from the top of that tree.

let parseInput (input : string seq) = 
    input |> parseArray2d |> Array2D.map charToInt

let rec isVisible (x,y) input (direction:(int*int)->(int*int)) (max:int) (visibleTrees:(int * int) list) : (int * int) list=
    let nextIdx = direction(x,y)
    if (Array2D.length1 input <= x || Array2D.length2 input <= y || x < 0 || y < 0) then
        visibleTrees
    else if (input[x,y] > max) then
        isVisible nextIdx input direction input[x,y] ((x,y)::visibleTrees)
    else
        isVisible nextIdx input direction max visibleTrees

let rec isVisibleFromTree (x,y) input (direction:(int*int)->(int*int)) (treeHeight:int) (visibleTrees:(int * int) list) : (int * int) list=
    let nextIdx = direction(x,y)
    if (Array2D.length1 input <= x || Array2D.length2 input <= y || x < 0 || y < 0) then
        visibleTrees
    else if (input[x,y] >= treeHeight) then
        ((x,y)::visibleTrees)
    else
        isVisibleFromTree nextIdx input direction treeHeight ((x,y)::visibleTrees)


let part1 (input:int array2d) =
    let height = Array2D.length1 input
    let width = Array2D.length2 input

    // Can probably be done in a better way, but this was the first that came to mind
    let mutable visibleTrees = []
    for i = 0 to height - 1 do
        visibleTrees <- isVisible (0, i) input (fun (x,y) -> (x+1,y)) -1 visibleTrees

    for i = 0 to height - 1 do
        visibleTrees <- isVisible (height - 1, i) input (fun (x,y) -> (x - 1,y)) -1 visibleTrees

    for i = 0 to height - 1 do
        visibleTrees <- isVisible (i, 0) input (fun (x,y) -> (x,y+1)) -1 visibleTrees

    for i = 0 to height - 1 do
        visibleTrees <- isVisible (i, width - 1) input (fun (x,y) -> (x,y-1)) -1 visibleTrees

    visibleTrees |> Set.ofList |> Set.count

let part2 (input:int array2d) =
    let scenicScore x y =
        if (Array2D.length1 input = x+1 || Array2D.length2 input = y+1 || x <= 0 || y <= 0) then
            0
        else
            let dir1 = isVisibleFromTree (x,y+1) input (fun (x,y) -> (x,y+1)) input[x,y] []
            let dir2 = isVisibleFromTree (x,y-1) input (fun (x,y) -> (x,y-1)) input[x,y] []
            let dir3 = isVisibleFromTree (x+1,y) input (fun (x,y) -> (x+1,y)) input[x,y] []
            let dir4 = isVisibleFromTree (x-1,y) input (fun (x,y) -> (x-1,y)) input[x,y] []

            let res = dir1.Length * dir2.Length * dir3.Length * dir4.Length
            res

    let scores = input |> Array2D.mapi (fun x y _ -> scenicScore x y) 

    //printfn "%A" scores

    scores |> array2dMax


let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "8" |> Async.RunSynchronously)
    Assert.Equal("1708", part1)
    Assert.Equal("504000", part2)