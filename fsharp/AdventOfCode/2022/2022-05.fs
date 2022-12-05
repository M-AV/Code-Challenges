module _2022_5

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Based on the original state of crates, follow the specified moves and see which are on top
// Task 2: Same as before, but allow multiple crates to be moved at the same time (affects ordering)

type Input = char list array * (int * int * int) list

let parseInput (input : string seq) = 
    // Use arrays for easy access by index later
    // 1. Find number of columns
    // 2. Run through each row from bottom up (so the stack is in the right order in the end)
    // 3. For each row, run through each column and add the letter if it has one
    let state = input |> Seq.takeWhile (fun x -> x <> "") |> Array.ofSeq
    let mutable columns = state[state.Length - 1].Split "   " |> Array.map int |> Array.map (fun x -> [])
    //printfn "%i" columns.Length
    for row = columns.Length - 2 downto 0 do
        for column in 0 .. columns.Length - 1 do
            let charIdx = 4 * (column + 1) - 3
            if charIdx < state[row].Length then
                columns[column] <- match state[row][charIdx] with
                                   | ' ' -> columns[column]
                                   | _ -> state[row][charIdx]::columns[column]
            
    //printfn "State: %A" columns

    let commands = 
        input 
        |> Seq.skipWhile (fun x -> x <> "") 
        |> Seq.skip 1 
        |> Seq.map (fun x -> x.Split([| "move "; " from "; " to " |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map int |> Array.ofSeq)
        |> Seq.map (fun x -> (x[0], x[1], x[2]))
        |> List.ofSeq
    
    //printfn "%A" commands

    Input(columns, commands)

let part1 (input : Input) =
    let rec solve (state:char list array) (remaining:(int * int * int) list) =
        match remaining with
        | [] -> state
        | head::tail -> 
            let (count, fromCol, toCol) = head
            let mutable newState = state

            for i = 1 to count do
                let toMove::remaining = (fst input)[fromCol - 1]
                newState[fromCol- 1] <- remaining
                newState[toCol - 1] <- toMove::newState[toCol - 1]

            solve newState tail

    let finishedState = solve (fst input) (snd input)

    //printfn "%A" finishedState
                
    finishedState |> Array.map (fun (x::rem) -> x) |> System.String // We assume none of the columns are empty
                
let part2 (input : Input) =
    let rec solve (state:char list array) (remaining:(int * int * int) list) =
        match remaining with
        | [] -> state
        | head::tail -> 
            let (count, fromCol, toCol) = head
            let mutable newState = state

            let mutable tempStack = []

            for i = 1 to count do
                let toMove::remaining = (fst input)[fromCol - 1]
                newState[fromCol - 1] <- remaining
                tempStack <- toMove::tempStack
            for s in tempStack do
                newState[toCol - 1] <- s::newState[toCol - 1]

            solve newState tail

    let finishedState = solve (fst input) (snd input)
                
    finishedState |> Array.map (fun (x::rem) -> x) |> System.String // We assume none of the columns are empty

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed
    let part2 = part2 (parseInput input) // Since we mutated the input in part 1, we just parse it again. A bit ugly, but whatever

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "5" |> Async.RunSynchronously)
    Assert.Equal("SHMSDGZVC", part1)
    Assert.Equal("VRZGHDFBQ", part2)