module _2021_4

open System

// Task 1: Bingo. Find winning board. Add all numbers not called and multiply with the latest number called.
// Task 2: 

let parseBoards lines =
    let parseLine (line:string) = 
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |>
        Seq.map int |>
        Seq.map (fun x -> x)

    // Batching logic from here: https://stackoverflow.com/a/7518857/1401257
    let batches = 
        lines |>
        Seq.mapi (fun idx value -> idx / 5, value) |>
        Seq.groupBy fst |>
        Seq.map snd |>          // Remove key
        Seq.map (Seq.map snd)   // Equivalent: Seq.map (fun x -> x |> Seq.map snd)

    let parsed =
        batches |>
        Seq.map (Seq.map parseLine) |>
        List.ofSeq

    parsed

let neededNumbersForRow (numbers : int seq) (row : int seq) =
    let rec findRequired (numbers: int seq) remaining count =
        match remaining with 
        | [] -> count
        | _ -> findRequired (numbers |> Seq.tail) (remaining |> List.filter (fun x -> x <> (numbers |> Seq.head))) (count + 1)

    let result = findRequired numbers (row |> List.ofSeq) 0
    result

let neededNumbersForBoard (numbers : int seq) (board : (int) seq seq) =
    let rec red rows (required: int) =
        match rows with 
        | [] -> required
        | _ -> red (rows |> List.tail) (min required (neededNumbersForRow numbers (rows |> Seq.head)))

    let horizontalResult = red (board |> List.ofSeq) Int32.MaxValue
    let verticalResult = red (board |> Seq.transpose |> List.ofSeq) Int32.MaxValue
    let result = min horizontalResult verticalResult
    result

let sumUnmarkedNumbers usedNumbers board =
    board |> Seq.collect (fun x -> x) |> Seq.except usedNumbers |> Seq.sum<int>

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let numbers = (Seq.head input).Split(',') |> Seq.map (fun x -> int x)
    let boards = input |> Seq.tail |> parseBoards

    let fastestBoard = 
        boards |> 
        Seq.map (fun board -> (neededNumbersForBoard numbers board, board)) |> 
        Seq.minBy fst
    
    let lastRequiredNumber = (numbers |> List.ofSeq)[(fst fastestBoard) - 1]
    let sumOfUnused = (sumUnmarkedNumbers (numbers |> Seq.take (fst fastestBoard)) (snd fastestBoard))

    //printfn "Sum %A" sumOfUnused
    //printfn "Board: %A" (snd fastestBoard)
    //printfn "Last number: %i" lastRequiredNumber

    let part1 = sumOfUnused * lastRequiredNumber

    let part2 = "N/A"

    part1.ToString(), part2.ToString()