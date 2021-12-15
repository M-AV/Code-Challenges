module _2021_2

// Task 1: Calculate horizontal x depth position after a series of moves
// Task 2: Some more complicated rules..

// (horizontal, depth)

let adjustPosition current direction distance =
    if direction = "forward" then
        ((fst current) + distance, snd current)
    else if direction = "down" then
        (fst current, (snd current) + distance)
    else if (direction = "up") then
        (fst current, (snd current) - distance)
    else
        current

let adjustPositionWithAim (horizontal, depth, aim) direction distance =
    match direction with 
        | "forward" -> (horizontal + distance, depth + aim * distance, aim)
        | "down" -> (horizontal, depth, aim + distance)
        | "up" -> (horizontal, depth, aim - distance)
        | _ -> (horizontal, depth, aim)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = 
        input |>
        Seq.map (fun s -> s.Split ' ') |>
        Seq.map (fun s -> s.[0], s.[1] |> int) 

    let part1 = 
        parsed |>
        Seq.fold (fun acc current -> adjustPosition acc (fst current) (snd current)) (0, 0)

    //printfn "%A" part1

    let part2 = 
        parsed |>
        Seq.fold (fun acc current -> adjustPositionWithAim acc (fst current) (snd current)) (0, 0, 0)
    let hor, depth, _ = part2

    (fst part1 * snd part1).ToString(), (hor * depth).ToString()