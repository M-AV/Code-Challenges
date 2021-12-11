module _2021_10

// Task 1: Ignore incomplete lines, find first invalid paranthesis and calculate score. Sum scores for result
// Task 2: Ignore corrupt lines, find missing brackets and calc score. Find middle score.

let openBrackets = [| '['; '('; '<'; '{' |]
let closeBrackets = [| ']'; ')'; '>'; '}' |]

let corruptedPointLookup char = 
    match char with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let missingPointLookup char =
    match char with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> 0

let isOpenBracket bracket = List.contains bracket  [ '['; '('; '<'; '{' ]
let isCloseBracket bracket = List.contains bracket [ ']'; ')'; '>'; '}' ]
let matches (first:char) (second:char) = (openBrackets |> Array.findIndex (fun x -> x = first)) = (closeBrackets |> Array.findIndex (fun x -> x = second) )
let getMatchingClose first = closeBrackets[openBrackets |> Array.findIndex (fun x -> x = first)]

let findFirstCorruptedChar (input: char list): char =
    let rec traverse input queue = 
        match input with 
        | head :: tail when isOpenBracket head -> traverse tail (head :: queue)
        | head :: tail when List.isEmpty queue -> head // If empty and we see a close bracket it's corrupted
        | head :: tail when head |> matches queue.Head -> traverse tail queue.Tail
        | [] -> '&' // Just return a random char for uncorrupted lines
        | head :: tail -> head
    
    traverse input []

let findMissingBrackets (input: char list): char list =
    let rec findOpen queue input : char list =
        match input with 
        | head :: tail when isOpenBracket head -> findOpen (head :: queue) tail
        | head :: tail when List.isEmpty queue -> []
        | head :: tail when head |> matches queue.Head -> findOpen queue.Tail tail
        | [] -> queue 
        | head :: tail -> [] // Just return empty list for corrupted lines

    input |> findOpen [] |> List.map getMatchingClose

let calcMissingBracketScore (input: char list) =
    input 
    |> List.map missingPointLookup
    |> List.reduce (fun agg x -> agg * 5L + x)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let part1 = 
        input 
        |> Seq.map List.ofSeq
        |> Seq.map findFirstCorruptedChar
        |> Seq.sumBy corruptedPointLookup

    let points = 
        input 
        |> Seq.map List.ofSeq
        |> Seq.map findMissingBrackets
        |> Seq.filter (fun x -> not (Seq.isEmpty x))
        |> Seq.map calcMissingBracketScore
        |> Seq.sort
        |> Array.ofSeq

    let part2 = points[points.Length / 2]

    part1.ToString(), part2.ToString()