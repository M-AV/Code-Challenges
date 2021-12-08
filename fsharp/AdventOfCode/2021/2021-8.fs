module _2021_8

// Task 1: Count words in output with length 2, 3, 4 or 7
// Task 2: 

let parseInput_Part1 (input : string seq) =
    input 
    |> Seq.map (fun x -> x.Split(" | "))
    |> Seq.map (fun x -> x[1])
    |> Seq.map (fun x -> x.Split(' '))

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    // This is the simplest possible way I can think of to solve part one. It makes no attempt at parsing
    // the values which I assume we need to do in part 2.
    let part1 = 
        input 
        |> parseInput_Part1 
        |> Seq.collect id
        |> Seq.filter (fun x -> (Array.contains x.Length [|2; 4; 3; 7|])) 
        |> Seq.length

    printfn "%A" part1

    let part2 = "N/A"

    part1.ToString(), part2.ToString()