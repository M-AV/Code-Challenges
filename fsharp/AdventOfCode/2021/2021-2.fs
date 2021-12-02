module _2021_2

// Task 1: Calculate horizontal x depth position after a series of moves
// Task 2: 

let adjustPosition current direction distance =
    if direction = "forward" then
        ((fst current) + distance, snd current)
    else if direction = "down" then
        (fst current, (snd current) + distance)
    else if (direction = "up") then
        (fst current, (snd current) - distance)
    else
        current

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let part1 = 
        input |>
        Seq.map (fun s -> s.Split ' ') |>
        Seq.map (fun s -> s.[0], s.[1] |> int) |>
        //Seq.fold (fun acc current -> 0, 0) (0, 0)
        Seq.fold (fun acc current -> adjustPosition acc (fst current) (snd current)) (0, 0)

    //printfn "%A" part1

    let part2 = "N/A"

    (fst part1 * snd part1).ToString(), part2.ToString()