module _2021_10

// Task 1: Ignore incomplete lines, find first invalid paranthesis and calculate score. Sum scores for result
// Task 2: 

let openBrackets = [ '['; '('; '<'; '{' ]
let closeBrackets = [ ']'; ')'; '>'; '}' ]

let pointLookup char = 
    match char with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let isOpenBracket bracket = List.contains bracket  [ '['; '('; '<'; '{' ]
let isCloseBracket bracket = List.contains bracket [ ']'; ')'; '>'; '}' ]
let matches (first:char) (second:char) = (openBrackets |> List.findIndex (fun x -> x = first)) = (closeBrackets |> List.findIndex (fun x -> x = second) )

let findFirstCorruptedChar (input: char list): char =
    let rec traverse input queue = 
        match input with 
        | head :: tail when isOpenBracket head -> traverse tail (head :: queue)
        | head :: tail when List.isEmpty queue -> head
        | head :: tail when head |> matches queue.Head -> traverse tail queue.Tail
        | [] -> '&' // Just return a random char for uncorrupted lines
        | head :: tail -> head
    
    traverse input []

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let part1 = 
        input 
        |> Seq.map List.ofSeq
        |> Seq.map findFirstCorruptedChar
        |> Seq.sumBy pointLookup

    let part2 = "N/A"

    part1.ToString(), part2.ToString()