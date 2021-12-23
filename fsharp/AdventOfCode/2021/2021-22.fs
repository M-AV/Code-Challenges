module _2021_22

open System

// Task 1: Flip bits in 3D 50x50x50 grid and count 1's afterwards
// Task 2: 99% sure part 2 is just saying "now consider the rest"..

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

let isWithin50 (state:CubeState) =
    let (_, (x_start, x_end), (y_start, y_end), (z_start, z_end)) = state
    x_end <= 50 && -50 <= x_start && 
    y_end <= 50 && -50 <= y_start && 
    z_end <= 50 && -50 <= z_start

let flipBitsInGrid (grid:int[,,]) state =
    let (newState, (x_start, x_end), (y_start, y_end), (z_start, z_end)) = state
    for i in x_start .. x_end do
        for j in y_start .. y_end do
            for l in z_start .. z_end do
                grid[i + 50,j + 50,l + 50] <- newState

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    let filtered = parsed |> List.filter isWithin50 

    let grid = Array3D.zeroCreate 100 100 100

    for action in filtered do
        flipBitsInGrid grid action

    //printfn "%A" filtered

    let part1 = 
        getAllElements grid 
        |> Seq.filter (fun x -> x = 1) 
        |> Seq.length

    let part2 = "N/A"

    part1.ToString(), part2.ToString()