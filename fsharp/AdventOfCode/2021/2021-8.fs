module _2021_8

// Task 1: Count words in output with length 2, 3, 4 or 7
// Task 2: Decode the 7 segment displays and add up all the final 4 digit numbers

let parseInput (input : string seq) =
    input
    |> Seq.map (fun x -> x.Split(" | "))
    |> Seq.map (fun x -> (x[0].Split(' '), x[1].Split(' ')))


// We don't have to map each line segment to find the number. We can compare the letters to 
// known letters figure it out that way. 

// How to identify
// 0 = Find all 6 letter words, if not 6 or 9 it must be 0
// 1 = 2 letters
// 2 = Find five letter words, if not 3, subtract 4, if 3 left it must be 2
// 3 = Find five letter words that use all of 1
// 4 = 4 letters
// 5 = Find five letter words, if not 2 or 3 it must be 5
// 6 = Find 6 letter words, subtract 1, if 5 left it must be 6
// 7 = 3 letters
// 8 = 7 letters
// 9 = Find all 6 letter words, subtract 4, if 2 left it must be 9
let solveSingleDisplay (input : string[] * string[]) =
    // We use the length as key. It might be more intuitive to have used the actual value, but it 
    // doesn't really matter
    let letterMap = 
        (fst input) 
        |> Array.map (fun x -> x |> Seq.toList) 
        |> Array.filter (fun x -> Array.contains x.Length [|2; 4; 3; 7|])
        |> Array.map (fun x -> (x.Length, x))
        |> Map.ofArray
    let outputValue = 
        (snd input)
        |> Array.map (fun x -> (x |> Seq.toList))
        |> Array.map (fun x -> 
            match x.Length with
            | 2 -> 1
            | 3 -> 7
            | 4 -> 4
            | 5 -> if (List.except x letterMap.[2]).Length = 0 then 3
                   else if (List.except letterMap.[4] x).Length = 3 then 2
                   else 5
            | 6 -> if (List.except letterMap.[2] x).Length = 5 then 6
                   else if (List.except letterMap.[4] x).Length = 2 then 9
                   else 0
            | 7 -> 8
            | _ -> 0) // Can't happen
    
    //printfn "%A" outputValue
    outputValue[0] * 1000 + outputValue[1] * 100 + outputValue[2] * 10 + outputValue[3]

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    // This is the simplest possible way I can think of to solve part one. It makes no attempt at parsing
    // the values which I assume we need to do in part 2.
    let part1 = 
        input 
        |> parseInput 
        |> Seq.map snd
        |> Seq.collect id
        |> Seq.filter (fun x -> (Array.contains x.Length [|2; 4; 3; 7|])) 
        |> Seq.length


    let parsedForPart2 = 
        input
        |> parseInput
        |> Seq.map solveSingleDisplay
        |> Seq.sum

    let part2 = parsedForPart2

    part1.ToString(), part2.ToString()