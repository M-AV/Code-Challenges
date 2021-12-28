module _2021_24

open System

// Task 1: Find max model number
// Task 2: Find min model number

// Yikes.. This one was tough. I'm not used to reverse engineering code or using 
// base X numbers as stacks, so not sure I would ever have solved this without 
// getting some inspiration from other people.

type Value = 
    | Variable of int
    | Integer of int

let varIndex var = 
    match var with
    | 'x' -> 0
    | 'y' -> 1
    | 'z' -> 2
    | 'w' -> 3
    | _ -> failwith "Invalid var"

let parseInput (input : string seq) = 
    let parseValue (value:string) =
        match Int32.TryParse value with
        | true, parsed -> Integer(parsed)
        | false, _ -> Variable((varIndex value[0]))
            
    input
    |> Seq.map (fun x -> x.Split ' ')
    |> Seq.map (fun x -> (x[0], varIndex (x[1][0]), if x.Length > 2 then parseValue x[2] else Integer(1)))

// Read instructions one by one and execute them 
let simulateInstructions (input:int array) program =
    let variables = Array.zeroCreate 4
    let mutable inputIdx = 0

    let getValue (value:Value) =
        match value with
        | Variable x -> variables[x]
        | Integer v -> v

    for (instr, var, value) in program do
        match instr with
        | "inp" -> 
            variables[var] <- input[inputIdx]
            inputIdx <- inputIdx + 1
        | "mul" -> variables[var] <- variables[var] * getValue(value)
        | "add" -> variables[var] <- variables[var] + getValue(value)
        | "div" -> variables[var] <- variables[var] / getValue(value)
        | "mod" -> variables[var] <- variables[var] % getValue(value)
        | "eql" -> variables[var] <- if variables[var] = getValue(value) then 1 else 0

    variables[varIndex 'z'] = 0

// Converted the instructions into F#. When doing it, it quickly becomes clear that
// we see the same "chunk" of code being executed repeatedly but with slightly different 
// hardcoded numbers.
let executeProgram2 (input: int array) =
    let mutable w = 0
    let mutable z = 0
    let mutable y = 0
    let mutable x = 0

    let xAddParam = [|12; 14; 11; -9; -7; 11; -1; -16; 11; -15; 10; 12; -4;  0|]
    let zDivParam = [| 1;  1;  1; 26; 26;  1; 26;  26;  1;  26;  1;  1; 26; 26|]
    let yAddParam = [|15; 12; 15; 12; 15; 2; 11; 15; 10; 2; 0; 0; 15; 15|]
    // This method should be called once for every input and paramIdx should increase by 1 each time
    let method input paramIdx =
        w <- input
        // x <- 0
        x <- z
        x <- x % 26
        z <- z / zDivParam[paramIdx]
        x <- x + xAddParam[paramIdx]
        //x <- if x = w then 1 else 0
        //x <- if x = 1 then 0 else 1
        x <- if x <> w then 1 else 0 // Should be same as commented lines above
        //y <- 0
        y <- 25
        y <- y * x
        y <- y + 1
        z <- z * y // Problematic if y is 26 ? I guess we would like input to be equal to X
        y <- y * 0
        y <- y + w
        y <- y + yAddParam[paramIdx]
        y <- y * x
        z <- z + y
    let method2 input paramIdx =
        w <- input
        x <- z % 26 
        z <- z / zDivParam[paramIdx]
        x <- x + xAddParam[paramIdx]
        x <- if x <> w then 1 else 0 // If x is 0 after this it resets y
        y <- (25 * x) + 1
        z <- z * y
        y <- (w + yAddParam[paramIdx]) * x
        z <- z + y

    for i in 0 .. input.Length - 1 do
        method2 input[input.Length - (i + 1)] i

    z = 0

let findHighestModelNumber program =
    let input = [|9;9;9;9;9;9;9;9;9;9;9;9;9;9|]
    let rec subtract1 idx =
        if input[idx] = 1 then
            input[idx] <- 9
            subtract1 (idx - 1)
        else 
            input[idx] <- input[idx] - 1

    let mutable foundRes = false
    let mutable res = input
    let mutable counter = 0L
    while (not foundRes) do
        counter <- counter + 1L
        //foundRes <- executeProgram input program
        foundRes <- executeProgram2 input
        if foundRes then
            res <- input
        else
            subtract1 13

    res

// As expected the above solution takes WAAAAY too long. With some threading and time we may be able to find a result, but
// there has to be another way....
// Observations
// - Only Z is being carried over for each iteration (w is overwritten with input, x is set to 0 and y is set to 25)
// - When xAdd is positive, zDiv is 1. When negative, zDiv is 26
// - We want x = w in the if-statement as it resets z and y and causes z to be 0 in the end
// - xAddParam = [10 .. 14] when zDiv is 1
// - yAddParam is at most 15 and our input is at most 9, so the max value when added is 25. This is why we use mod/div 26

// In the end I gave up and searched for explanations elsewhere. Found this excellent explanation: https://github.com/mrphlip/aoc/blob/master/2021/24.md
// After reading a few sentences it dawned on me what I should look for.

// We're using a base-26 number as a stack and compare later input numbers with earlier ones (+- some offset)
// Translated into more readable code, it looks something like this (z is our stack):
// x = (peek z) + (xAdd)       // Observation: Will always be negative when relevant
// if (idx in [3,4, 6,7,9, 12, 13])
//     pop z
// If x <> w then              // Observation: We pop 7 times, so I we want to push 7 times as well - hence on every pop we want them to match
//     push (w + yAdd) to stack
// else 
//     keep z as is
// 
// That means we have the following rules: 
// [|15; 12; 15; 12; 15; 2; 11; 15; 10; 2; 0; 0; 15; 15|]
// let xAddParam = [|12; 14; 11; -9; -7; 11; -1; -16; 11; -15; 10; 12; -4;  0|]
// input[3] - 9  = input[2] + 15   -> input[3] = input[2] + 6
// input[4] - 7  = input[1] + 12   -> input[4] = input[1] + 5
// input[6] - 1  = input[5] + 2    -> input[6] = input[5] + 1
// input[7] - 16 = input[0] + 15   -> input[7] = input[0] - 1
// input[9] - 15 = input[8] + 10   -> input[9] = input[8] - 5
// input[12] - 4 = input[11] + 0   -> input[12] = input[11] - 4
// input[13] - 0 = input[10] + 0   -> input[13] = input[10]

let findModelNumber minMax =
    // 0 = [2..9]
    // 1 = [1..4]
    // 2 = [1..3]
    // 3 = [7..9]
    // 4 = [6..9]
    // 5 = [1..8]
    // 6 = [2..9]
    // 7 = [1..8]
    // 8 = [6..9]
    // 9 = [1..4]
    // 10 = [1..9] (but same as 13)
    // 11 = [5..9]
    // 12 = [1..5]
    // 13 = [1..9] (but same as 10)
    let isValidModelNumber (input:int array)  =
        input[3] = input[2]+6 && 
        input[4] = input[1]+5 &&
        input[6] = input[5]+1 &&
        input[7] = input[0]-1 &&
        input[9] = input[8]-5 &&
        input[12] = input[11]-4 &&
        input[13] = input[10]               

    // Based on above rules we can construct a max and min value for each digit in the input.
    // Interestingly, these are also the max/min values we're searching for (but I wanted the 
    // code to "work" - not just return hardcoded values - so I implemented the search 
    // functionality anyways)
    let maxVals = [|9;4;3;9;9;8;9;8;9;4;9;9;5;9|]
    let minVals = [|2;1;1;7;6;1;2;1;6;1;1;5;1;1|]
    let reset = if minMax = "max" then maxVals else minVals
    let compare = if minMax = "max" then minVals else maxVals
    let input =  if minMax = "max" then [|9;4;3;9;9;8;9;8;9;4;9;9;5;9|] else [|2;1;1;7;6;1;2;1;6;1;1;5;1;1|]
    let addSub = if minMax = "max" then (fun x -> x - 1) else (fun x -> x + 1)
    let rec subtract1 idx =
        if input[idx] = compare[idx] then
            input[idx] <- reset[idx]
            subtract1 (addSub idx)
        else 
            input[idx] <- input[idx] - 1

    let mutable foundRes = false
    let mutable res = input
    while (not foundRes) do
        foundRes <- isValidModelNumber input
        if foundRes then
            res <- input
        else
            subtract1 13
    
    res |> Array.fold (fun (agg:int64) cur -> agg * 10L + (int64 cur)) 0L

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    // We don't need the parsed value anymore, as it represents code and 
    // we have rewritten that code in F#.
    let parsed = parseInput input

    let part1 = findModelNumber "max"
    let part2 = findModelNumber "min"

    part1.ToString(), part2.ToString()