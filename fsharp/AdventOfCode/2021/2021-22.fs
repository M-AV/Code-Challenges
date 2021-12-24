module _2021_22

open System
open Xunit

// Task 1: Flip bits in 3D 50x50x50 grid and count 1's afterwards
// Task 2: Same same, but the whole grid

type CubeState = int * (int * int) * (int * int) * (int * int)

let parseInput (input: string seq) = 
    let parseState state =
        match state with
        | "on" -> 1
        | "off" -> 0
        | _ -> failwith "Invalid state"

    let parseArea (area:string) =
        let parseDimension (dim:string) =
            let tmp = dim.Split("..")
            (int (tmp[0].[2..]), int tmp[1])
        let dimensions = 
            area.Split(',')
            |> Array.map parseDimension
        dimensions
        
    input 
    |> Seq.map (fun x -> x.Split ' ')
    |> Seq.map (fun x -> (parseState x[0], parseArea x[1]))
    |> Seq.map (fun (s, cube) -> CubeState(s, cube[0], cube[1], cube[2]))
    |> List.ofSeq

let getAllElements (a:int[,,]) : int seq =
    seq { for i in 0 .. a.GetLength(0)-1 do
          for j in 0 .. a.GetLength(1)-1 do 
          for l in 0 .. a.GetLength(2)-1 do
            yield a.[i,j,l] }

let isOverlapping (first:CubeState) (second:CubeState) =
    let (_, (x1_start, x1_end), (y1_start, y1_end), (z1_start, z1_end)) = first
    let (_, (x2_start, x2_end), (y2_start, y2_end), (z2_start, z2_end)) = second

    x1_start <= x2_end && x2_start <= x1_end && 
    y1_start <= y2_end && y2_start <= y1_end && 
    z1_start <= z2_end && z2_start <= z1_end

// Is started on the complicated solution first, because I kinda new it was coming..
// but then I thought I might as well just solve it the easy way and see if I was correct
let flipBitsInGrid (grid:int[,,]) state =
    let (newState, (x_start, x_end), (y_start, y_end), (z_start, z_end)) = state
    for i in x_start .. x_end do
        for j in y_start .. y_end do
            for l in z_start .. z_end do
                grid[i + 50,j + 50,l + 50] <- newState

// For part 2, the grid is too big to create. So I thought I would keep track of all cubes
// and for each new action split them appropriately. It would then give me a list (just like the 
// input) of all non-overlapping cubes and their states.
// It takes almost 20 minutes to run on my PC... So it could use some optimizations.
let cutAxisAtOverlap ((x1_start, x1_end):int*int) ((x2_start, x2_end):int*int) =
    if (x1_start, x1_end) = (x2_start, x2_end) then
        ([], (x1_start, x1_end)), ([], (x2_start, x2_end))
    // If 2 is fully contained in 1
    else if x1_start <= x2_start && x2_end <= x1_end then 
        let mutable nonOverlapping = []
        if x1_start = x2_start then
            nonOverlapping <- [ (x2_end + 1, x1_end) ]
        else if x2_end = x1_end then
            nonOverlapping <- [( x1_start, x2_start - 1)]
        else 
            nonOverlapping <- [ ( x1_start, x2_start - 1); (x2_end + 1, x1_end) ]
        let overlapping = (x2_start, x2_end)

        ((nonOverlapping, overlapping), ([], overlapping))
    // If 1 is fully container in 2
    else if x2_start <= x1_start && x1_end <= x2_end then 
        let mutable nonOverlapping = []
        if x2_start = x1_start then
            nonOverlapping <- [(x1_end + 1, x2_end)]
        else if x1_end = x2_end then
            nonOverlapping <- [( x2_start, x1_start - 1)]
        else 
            nonOverlapping <- [ ( x2_start, x1_start - 1); (x1_end + 1, x2_end) ]

        let overlapping = (x1_start, x1_end)

        (([], overlapping), (nonOverlapping, overlapping)) // Keep current, split new into 3
    // If 1 starts before 2
    else if x1_start < x2_start then
        (
            ([ (x1_start, x2_start - 1) ] , (x2_start, x1_end)),
            ([ (x1_end + 1, x2_end) ], (x2_start, x1_end))
        )
    // If 2 starts before 1
    else 
        (
            ([ (x2_end + 1, x1_end) ], (x1_start, x2_end)),
            ([ (x2_start, x1_start - 1) ], (x1_start, x2_end))
        )

let rec mergeOverlapping (toUpdate:CubeState) (newCube:CubeState) =
    let (state_c, x_c, y_c, z_c) = toUpdate
    let (state_n, x_n, y_n, z_n) = newCube
    if not (isOverlapping toUpdate newCube) then
        ([toUpdate], [newCube])
    else if x_c = x_n && y_c = y_n && z_c = z_n then
        ([newCube], List.empty)
    else
        let mutable mergedCurrent = List.empty
        let mutable mergedNew = List.empty

        // Split on x
        let ((safe_c, overlapping_x_c), (safe_n, overlapping_x_n)) = cutAxisAtOverlap x_c x_n
        let mutable safeCurrent = safe_c |> List.map (fun x -> CubeState(state_c, x, y_c, z_c))
        let mutable safeNew = safe_n |> List.map (fun x -> CubeState(state_n, x, y_n, z_n))
        mergedCurrent <- (mergedCurrent @ safeCurrent)
        mergedNew <- (mergedNew @ safeNew)
        let ((safe_c, overlapping_y_c), (safe_n, overlapping_y_n)) = cutAxisAtOverlap y_c y_n

        safeCurrent <- safe_c |> List.map (fun y -> CubeState(state_c, overlapping_x_c, y, z_c))
        safeNew <- safe_n |> List.map (fun y -> CubeState(state_n, overlapping_x_n, y, z_n))
        mergedCurrent <- (mergedCurrent @ safeCurrent)
        mergedNew <- (mergedNew @ safeNew)
        
        let ((safe_c, overlapping_z_c), (safe_n, overlapping_z_n)) = cutAxisAtOverlap z_c z_n

        safeCurrent <- safe_c |> List.map (fun z -> CubeState(state_c, overlapping_x_c, overlapping_y_c, z))
        safeNew <- safe_n |> List.map (fun z -> CubeState(state_n, overlapping_x_n, overlapping_y_n, z))
        mergedCurrent <- (mergedCurrent @ safeCurrent)
        mergedNew <- (mergedNew @ safeNew)

        let overwritten = CubeState(state_n, overlapping_x_n, overlapping_y_n, overlapping_z_n)

        (overwritten::mergedCurrent, mergedNew)
        

let rec flipBits (currentStates:CubeState list) (toBeMatched:CubeState list) = 
    let rec matchWithCurrentStates toMatch states result=
        match states with
        | [] -> (toMatch::result, [])
        | head::tail when isOverlapping head toMatch ->
            let (split, newCubes) = mergeOverlapping head toMatch
            (split@result@tail, newCubes)
        | head::tail ->
            matchWithCurrentStates toMatch tail (head::result)

    match toBeMatched with
    | [] -> currentStates
    | newState::remainingNewStates ->
        let (updated, newCubes) = matchWithCurrentStates newState currentStates []
        flipBits updated (newCubes@remainingNewStates)
        

let size (cube:CubeState) : int64 = 
    let (_, (x1_start, x1_end), (y1_start, y1_end), (z1_start, z1_end)) = cube
    int64(x1_end - x1_start + 1) * int64(y1_end - y1_start + 1) * int64(z1_end - z1_start + 1)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    let filtered = parsed |> List.filter (isOverlapping (0, (-50,50), (-50,50), (-50,50)))

    let grid = Array3D.zeroCreate 100 100 100

    for action in filtered do
        flipBitsInGrid grid action

    //printfn "%A" filtered

    let part1 = 
        getAllElements grid 
        |> Seq.filter (fun x -> x = 1) 
        |> Seq.length

    let asfasd = flipBits List.empty parsed

    //printfn "%A" asfasd

    let part2 = 
        asfasd
        |> List.filter (fun (state, _,_,_) -> state = 1)
        |> List.map size
        |> List.sum
        

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Split``() =
    let split = cutAxisAtOverlap (1,3) (1, 2)
    let i = 0
    2