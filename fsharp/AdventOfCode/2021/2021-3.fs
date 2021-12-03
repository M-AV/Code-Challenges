module _2021_3

open System

// Task 1: Merge bits by most common bit in a given position and calculate resulting int value and negated value (you don't have 32 bits)
// Task 2: 

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = 
        input |>
        Seq.map (fun s -> s |> Seq.map (fun c -> c |> Char.GetNumericValue |> int)) // Parse ints

    let initial = Seq.replicate (Seq.head parsed |> Seq.length) 0
    let mapped = 
        parsed |>
        Seq.map (fun s -> s |> Seq.map (fun c -> if c = 0 then -1 else c)) |> // Adjust 0s to start with -1
        Seq.fold (fun acc curr -> Seq.zip acc curr |> Seq.map (fun (fst, snd) -> fst+snd)) initial |>
        Seq.map (fun x -> if x >= 0 then 1 else 0)

    let (gamma, _, _) = 
        mapped |>
        Seq.fold (fun (value, cnt, length) curr -> (value ||| (curr <<< (length - cnt - 1)), cnt + 1, length)) (0, 0, Seq.length initial)
    let (epsilon, _, _) = 
        mapped |>
        Seq.map (fun x -> if x = 1 then 0 else 1) |>
        Seq.fold (fun (value, cnt, length) curr -> (value ||| (curr <<< (length - cnt - 1)), cnt + 1, length)) (0, 0, Seq.length initial)
    let part1 = gamma * epsilon
    //printfn "%i" epsilon

    let part2 = "N/A"

    part1.ToString(), part2.ToString()