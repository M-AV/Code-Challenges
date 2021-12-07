module _2021_7

// Task 1: Find minimum number of adjustments crabs need to make to be aligned
// Task 2: Find minimum number but fuel is exponential

let parseInitialState (line:string) =
    line.Split(',') |> Array.map int

let findMinimumMoves_ConstantFuelBurn (positions:int list) =
    let fuelNeeded positions target =
        positions |> 
        List.map (fun x -> (max target x) - (min target x)) |> 
        List.sum

    let lowest = List.min positions
    let highest = List.max positions

    [ for i in lowest .. highest -> i ] |>
        List.map (fun x -> fuelNeeded positions x) |>
        List.min

let findMinimumMoves_ExponentialFuelBurn(positions:int list) =
    // Formula: 0.5 * n * (n + 1)
    let fuelNeeded positions target =
        positions |> 
        List.map (fun position -> (max target position) - (min target position)) |>
        List.map (fun moves -> (moves * (moves + 1)) >>> 1) |>
        List.sum

    let lowest = List.min positions
    let highest = List.max positions

    [ for i in lowest .. highest -> i ] |>
        List.map (fun x -> fuelNeeded positions x) |>
        List.min

let execute (input : string seq) =
    
    let parsed = 
        input |> 
        Seq.map parseInitialState |> List.ofSeq |> List.head |> List.ofSeq |> List.sort

    printfn "Input count: %i" (Seq.length parsed)

    let part1 = findMinimumMoves_ConstantFuelBurn parsed

    let part2 = findMinimumMoves_ExponentialFuelBurn parsed

    part1.ToString(), part2.ToString()