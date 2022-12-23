module _2021_4

open System
open InputProvider
open Xunit

// Task 1: Bingo. Find winning board. Add all numbers not called and multiply with the latest number called.
// Task 2: Find loosing board and do the same

let parseBoards lines =
    let parseLine (line:string) = 
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |>
        Seq.map int |>
        Seq.map (fun x -> x) |>
        List.ofSeq

    // Batching logic from here: https://stackoverflow.com/a/7518857/1401257
    let batches = 
        lines |>
        Seq.filter (fun x -> x <> "") |>
        Seq.mapi (fun idx value -> idx / 5, value) |>
        Seq.groupBy fst |>
        Seq.map snd |>          // Remove key
        //Seq.map (Seq.map snd)   // Equivalent: Seq.map (fun x -> x |> Seq.map snd)
        Seq.map (fun x -> x |> Seq.map snd |> List.ofSeq) |>
        List.ofSeq

    let parsed =
        batches |>
        Seq.map (List.map parseLine) |>
        List.ofSeq

    parsed

let neededNumbersForRow (numbers : int list) (row : int list) =
    let rec findRequired (numbers: int list) remaining count =
        match remaining with 
        | [] -> count
        | _ -> findRequired (numbers |> List.tail) (remaining |> List.filter (fun x -> x <> (numbers |> Seq.head))) (count + 1)

    let result = findRequired numbers (row) 0
    result

let minNeededNumbersForBoard (numbers : int list) (board : (int) list list) =
    let rec red rows (required: int) =
        match rows with 
        | [] -> required
        | _ -> red (rows |> List.tail) (min required (neededNumbersForRow numbers (rows |> List.head)))

    let horizontalResult = red (board) Int32.MaxValue
    let verticalResult = red (board |> List.transpose) Int32.MaxValue
    let result = min horizontalResult verticalResult
    result


let sumUnmarkedNumbers usedNumbers board =
    board |> Seq.collect (fun x -> x) |> Seq.except usedNumbers |> Seq.sum<int>

let findLastRequiredAndSumOfUnused numbers boards comparer =
    let board = 
        boards |> 
        Seq.map (fun board -> (minNeededNumbersForBoard numbers board, board)) |> 
        comparer fst
    
    let lastRequiredNumber = (numbers)[(fst board) - 1]
    let sumOfUnused = (sumUnmarkedNumbers (numbers |> List.take (fst board)) (snd board))
    (lastRequiredNumber, sumOfUnused)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let numbers = (Seq.head input).Split(',') |> Seq.map (fun x -> int x) |> List.ofSeq
    let boards = input |> Seq.tail |> parseBoards

    // This is a quite slow calculation.. So i'm sure it can be done in a better way

    let (lastRequiredNumber, sumOfUnused) = findLastRequiredAndSumOfUnused numbers boards Seq.minBy

    //printfn "Sum %A" sumOfUnused
    //printfn "Board: %A" (snd fastestBoard)
    //printfn "Last number: %i" lastRequiredNumber

    let part1 = sumOfUnused * lastRequiredNumber

    let (lastRequiredNumber, sumOfUnused) = findLastRequiredAndSumOfUnused numbers boards Seq.maxBy

    let part2 = sumOfUnused * lastRequiredNumber

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2021" "4" |> Async.RunSynchronously)
    Assert.Equal("64084", part1)
    Assert.Equal("12833", part2)