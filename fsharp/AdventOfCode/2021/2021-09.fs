module _2021_9

open InputProvider
open Xunit

// Task 1: Find low points in a number grid
// Task 2: Find size of 3 biggest 'basins' and multiply them

let parseInput (input: string seq) =
    let lineLength = (Seq.head input).Length

    let parsedLines = 
        input 
        |> Seq.collect (fun x -> x |> Seq.map int)
        |> Seq.map (fun x -> x - (int '0'))
        |> Array.ofSeq

    (parsedLines, lineLength)

let valAbove (input:int array, index:int, lineLength:int) = 
    if index - lineLength < 0 then (-1, 99999999)
    else (index - lineLength, input[index - lineLength])
let valBelow(input:int array, index:int, lineLength:int) = 
    if index + lineLength > (Array.length input) then (-1, 99999999)
    else (index + lineLength, input[index + lineLength])
let valPrev(input:int array, index:int, lineLength:int) =
    if index % lineLength = 0 then (-1, 99999999)
    else (index - 1, input[index - 1])
let valNext(input:int array, index:int, lineLength:int) =
    if index = 0 then (1, input[1])
    else if (index + 1) % lineLength = 0 then (-1, 99999999)
    else (index + 1, input[index + 1])

let isLowPoint input lineLength idx value  =
    value < snd(valAbove(input, idx, lineLength)) && 
    value < snd(valNext(input, idx, lineLength)) && 
    value < snd(valBelow(input, idx, lineLength)) && 
    value < snd(valPrev(input, idx, lineLength))

let countLowPoints (input:int array, lineLength:int) = 
    input 
    |> Seq.indexed
    |> Seq.filter (fun (idx, x) -> isLowPoint input lineLength idx x)
    |> Seq.map (fun (_, x) -> x + 1)
    |> Seq.sum

let findBasins (input: int array, lineLength:int) =
    let rec findBasin idx prevIdx result : int Set =
        if Set.contains idx result then Set.empty // We only do this to avoid going on paths already visited (even though the set would remove duplicates)
        else
            match idx with
            | -1 -> Set.empty
            | x when input[idx] = 9 -> Set.empty
            | x when prevIdx >= 0 && input[idx] <= input[prevIdx] -> Set.empty
            | _ ->  let result = Set.empty.Add(idx)
                    result 
                    |> Set.union (findBasin (fst(valAbove(input, idx, lineLength))) idx result)
                    |> Set.union (findBasin (fst(valNext(input, idx, lineLength))) idx result)
                    |> Set.union (findBasin (fst(valBelow(input, idx, lineLength))) idx result)
                    |> Set.union (findBasin (fst(valPrev(input, idx, lineLength))) idx result)

    let lowPoints =
        input 
        |> Seq.indexed
        |> Seq.filter (fun (idx, x) -> isLowPoint input lineLength idx x)
        |> Seq.map fst

    //printfn "%A" (lowPoints |> List.ofSeq)

    lowPoints |> Seq.map (fun x -> findBasin x -1 Set.empty)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let (values, lineLength) = input |> parseInput

    //printfn "%A" parsed

    let part1 = countLowPoints (values, lineLength)

    let basins = findBasins (values, lineLength)

    let part2 = 
        basins 
        |> Seq.sortByDescending Seq.length 
        |> Seq.take 3 
        |> Seq.map Seq.length 
        |> Seq.reduce (fun agg x -> agg * x)

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2021" "9" |> Async.RunSynchronously)
    Assert.Equal("465", part1)
    Assert.Equal("1269555", part2)