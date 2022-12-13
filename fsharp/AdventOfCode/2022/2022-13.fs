module _2022_13

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Text.RegularExpressions

// Task 1: Compare lines in pairs as described in problem and sum the indexes (1-based) of the packet pairs that are in the right order
// Task 2: Adding two new lines (hardcoded), order all the lines and multiply the indexes of the two added lines.

type Item =
    | Literal of int
    | List of Item list

// To parse each line we:
// Use a loop to parse every item in a list. If an item is itself a list, we recursively parse that item 
let parseInput (input : string seq) = 
    let rec parseTree line idx : Item * int =
        let mutable innerIdx = idx
        let mutable result:(Item list) = []
        let mutable notDone = true
        while innerIdx < (String.length line) && notDone do
            match line[innerIdx] with
            | '[' -> 
                let (subItem, newIdx) = parseTree line (innerIdx+1)
                result <- subItem::result
                innerIdx <- newIdx
            | ']' ->
                notDone <- false
            | x when Char.IsDigit x ->
                let digitEndIdx = innerIdx + (line[innerIdx..].IndexOfAny [| ','; ']' |])
                let digit = Literal(int line[innerIdx..(digitEndIdx-1)])
                result <- digit::result
                innerIdx <- digitEndIdx
            | ',' ->
                innerIdx <- innerIdx+1

        (List(result |> List.rev), innerIdx + 1)

    input 
    |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace(x)))
    |> Seq.map (fun x -> parseTree x 0)
    |> Seq.map fst

// Recursive search that stops when we know if something is in the right order
let rec isInOrder (left, right) =
    match (left, right) with
    // If both literals, just compare
    | (Literal x, Literal y) -> 
        if (x < y) then Some(true)
        elif (x > y) then Some(false)
        else None
    // If 1 is literal and the other a list, wrap literal in list and compare
    | (Literal x, List y) -> 
        isInOrder (List([left]), right)
    | (List x, Literal y) ->
        isInOrder (left, List([right]))
    // If both are lists, compare first item. If inconclusive, compare remaining
    | (List x, List y) ->
        match (x,y) with
        | ([],[]) -> None
        | (x, []) -> Some(false)
        | ([], y) -> Some(true)
        | (x::xRest, y::yRest) ->
            match isInOrder (x,y) with
            | None -> isInOrder (List(xRest), List(yRest))
            | x -> x

let part1 input =
    input
    |> batchesOf2
    |> Seq.map isInOrder 
    |> Seq.indexed 
    |> Seq.filter (fun x -> (snd x).IsSome)
    |> Seq.filter (fun (idx,x) -> x.Value) 
    |> Seq.map (fun (x,y) -> x+1) // 1 based idx
    |> Seq.sum

let part2 input =
    let added1 = parseInput [ "[[2]]" ] |> Seq.exactlyOne
    let added2 = parseInput [ "[[6]]" ] |> Seq.exactlyOne
    let items = added1::added2::(input |> List.ofSeq)
    
    let ordered = 
        items
        |> Seq.indexed
        |> Seq.sortWith (fun (idxL, left) (idxR, right) -> 
            match (isInOrder (left, right)) with
            | Some x when x = true -> -1
            | Some x when x = false -> 1
            | None -> 0)

    let idx1 = ordered |> Seq.findIndex (fun (idx, _) -> idx = 0)
    let idx2 = ordered |> Seq.findIndex (fun (idx, _) -> idx = 1)
    (idx1+1) * (idx2+1)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed
    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "13" |> Async.RunSynchronously)
    Assert.Equal("5625", part1)
    Assert.Equal("23111", part2)