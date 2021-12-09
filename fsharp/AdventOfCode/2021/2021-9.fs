module _2021_9

// Task 1: Find low points in a number grid
// Task 2: 

let parseInput (input: string seq) =
    let lineLength = (Seq.head input).Length

    let parsedLines = 
        input 
        |> Seq.collect (fun x -> x |> Seq.map int)
        |> Seq.map (fun x -> x - (int '0'))
        |> Array.ofSeq

    (parsedLines, lineLength)

let countLowPoints (input:int array, lineLength:int) = 
    let valAbove index = 
        if index - lineLength < 0 then 99999999
        else input[index - lineLength]
    let valBelow index = 
        if index + lineLength > (Array.length input) then 99999999
        else input[index + lineLength]
    let valPrev index =
        if index % lineLength = 0 then 99999999
        else input[index - 1]
    let valNext index =
        if index = 0 then input[1]
        else if (index + 1) % lineLength = 0 then 99999999
        else input[index + 1]
    let isLowPoint idx value =
        value < valAbove(idx) && 
        value < valNext(idx) && 
        value < valBelow(idx) && 
        value < valPrev(idx)
        
        
    input 
    |> Seq.indexed
    |> Seq.filter (fun (idx, x) -> isLowPoint idx x)
    |> Seq.map (fun (_, x) -> x + 1)
    |> Seq.sum

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let (values, lineLength) = input |> parseInput

    //printfn "%A" parsed

    let part1 = countLowPoints (values, lineLength)

    let part2 = "N/A"

    part1.ToString(), part2.ToString()