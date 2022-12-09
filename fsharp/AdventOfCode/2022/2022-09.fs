module _2022_9

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Simulate all moves in the input and figure out how many unique cells the following tail is visiting (based on the rules specified in the description)
// Task 2: Snake time! Instead of 1 point following the head, we now have 10 in a sequence. Each following the point in front. We still need to find unique cells 
//         visited by the last point.

let parseInput (input : string seq) = 
    input |> Seq.map (fun x -> x.Split ' ') |> Seq.map (fun x -> (x[0][0], x[1] |> int)) |> List.ofSeq

let findNewPosition (hx,hy) (tx,ty) =
    let shouldNotMove = hx-1 <= tx && tx <= hx+1 && hy-1 <= ty && ty <= hy+1
    match (tx,ty) with
    | p when shouldNotMove -> p
    | p when tx > hx && ty > hy -> (tx-1, ty-1)
    | p when tx > hx && ty = hy -> (tx-1, ty)
    | p when tx > hx && ty < hy -> (tx-1, ty+1)

    | p when tx < hx && ty > hy -> (tx+1, ty-1)
    | p when tx < hx && ty = hy -> (tx+1, ty)
    | p when tx < hx && ty < hy -> (tx+1, ty+1)

    | p when tx = hx && ty < hy -> (tx, ty+1)
    | p when tx = hx && ty > hy -> (tx, ty-1)

// It should be fairly easy to adjust part 1 to use the same function as part2 (just use an array of 2 elements), 
// but this is how I solved them so I'll keep it as is.
let part1 input = 
    let mutable visited = Set.empty |> Set.add (0,0)
    let simulateSingleStep (direction:char) (hx:int,hy:int) (tx:int,ty:int) : (int * int) * (int * int) =
        let newHeadPos = 
            match direction with
            | 'U' -> (hx, hy-1)
            | 'D' -> (hx, hy+1)
            | 'L' -> (hx-1, hy)
            | 'R' -> (hx+1, hy)

        let newTailPos = findNewPosition newHeadPos (tx,ty)
        (newHeadPos, newTailPos)

    let rec solve remaining headPos tailPos =
        let mutable head = headPos;
        let mutable tail = tailPos;
        match remaining with
        | [] -> ignore
        | (dir, count)::rem ->
            for i = 1 to count do   
                
                let (h,t) = simulateSingleStep dir head tail
                head <- h
                tail <- t
                visited <- visited |> Set.add(tail)
            solve rem head tail

    solve input (0,0) (0,0)

    visited |> Set.count

let part2 input = 
    let mutable visited = Set.empty |> Set.add (0,0)
    let moveHead direction (hx,hy) =
        match direction with
        | 'U' -> (hx, hy-1)
        | 'D' -> (hx, hy+1)
        | 'L' -> (hx-1, hy)
        | 'R' -> (hx+1, hy)


    let rec solve remaining (positions:(int * int) array) =
        match remaining with
        | [] -> ignore
        | (dir, count)::rem ->
            for i = 1 to count do
                positions[0] <- moveHead dir positions[0]

                for j = 1 to (positions |> Array.length)-1 do
                    positions[j] <- findNewPosition positions[j-1] positions[j]

                visited <- visited |> Set.add(positions |> Array.last)

            solve rem positions

    solve input (Array.init 10 (fun x -> (0,0)))

    visited |> Set.count

    
let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "9" |> Async.RunSynchronously)
    Assert.Equal("6563", part1)
    Assert.Equal("2653", part2)