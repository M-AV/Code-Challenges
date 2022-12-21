    module _2022_21

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Solve the equation for 'root'
// Task 2: Change 'root' to be a comparison between 2 values. Find out what the value of 'humn' needs to be for the two values to be equal

type Operation = 
    | Number of int64
    | Op of string * string * char

// Parse into a tuple with a name and the Operation type above.
let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> x.Split ": ") 
    |> Seq.map (fun x -> x[0], x[1])
    |> Seq.map (fun (n,x) -> 
        match Int64.TryParse x with
        | true, int -> n, Number(int)
        | _ -> 
            let first = x.Substring(0, 4)
            let op = x[5]
            let snd = x.Substring(7, 4)
            n, Op(first, snd, op))
    |> List.ofSeq

// Run through each item and
//   - If it's a number, just add it to the result map
//   - If it's an operation and we know the two values, calculate and add to result map
//   - If we don't know the values, add them to a list of unknowns
//   - In the end, rerun the calculation on the unknowns as we might be able to calculate them know
//   - Keep going until all are done or we can't calculate anymore
let rec calc lastRem result remaining remainingTwo : Map<string, int64> * (string * Operation) list =
    match remaining with
    | [] -> 
        let remainingCnt = remainingTwo |> List.length
        if remainingCnt > 0 && lastRem <> remainingCnt then
            calc remainingCnt result remainingTwo []
        else 
            result, remainingTwo
    | (name, value)::tail ->
        match value with
        | Number x -> 
            calc lastRem (result |> Map.add name x) tail remainingTwo
        | Op (first, second, op) -> 
            if (result.ContainsKey first && result.ContainsKey second) then
                let firstVal = result[first]
                let secondVal = result[second]
                match op with 
                | '*' -> calc lastRem (result |> Map.add name (firstVal * secondVal)) tail remainingTwo
                | '/' -> calc lastRem (result |> Map.add name (firstVal / secondVal)) tail remainingTwo
                | '+' -> calc lastRem (result |> Map.add name (firstVal + secondVal)) tail remainingTwo
                | '-' -> calc lastRem (result |> Map.add name (firstVal - secondVal)) tail remainingTwo
            else
                calc lastRem result tail ((name,value)::remainingTwo)

// 1. Sort input so all known values are first (I initially thought of this as a small optimization, but not sure it actually matters)
// 2. Run calculation and check value of root
let part1 input =
    // Ugly sort, but whatevs
    let sorted = 
        (input |> List.filter (fun (_,x) -> match x with | Number _ -> true | _ -> false))
        @
        (input |> List.filter (fun (_,x) -> match x with | Number _ -> false | _ -> true))

    let (calculated, _) = calc 0 Map.empty sorted []

    calculated["root"]

// Initially I tried to brute force the answer.. After 30 minutes of executing I gave up on that idea. xD
// Instead we have to solve the equation by reducing it down to 'humn = x'
//
// ## Here I confirmed that only 1 variable of 'root' is actually dependent on 'humn' and made the assumption that 
// ## each calculation would have 1 known value and 1 depending on 'humn'. This simplifies the logic. I am happy I was right.
// 
// 1. Sort list as before
// 2. Filter out 'humn' and 'root'
// 3. Run calculation to get as many known values as possible
// 5. Find the known and unknown side of the 'root' comparison, 
// 6. Reduce equation using the solve method
let part2 input =
    let sorted = 
        (input |> List.filter (fun (_,x) -> match x with | Number _ -> true | _ -> false) |> List.filter (fun (n,_) -> n <> "humn"))
        @
        (input |> List.filter (fun (_,x) -> match x with | Number _ -> false | _ -> true) |> List.filter (fun (n,_) -> n <> "root"))
        
    let (r, rootOp) = input |> List.filter (fun (n,_) -> n = "root") |> List.exactlyOne
    let monkeys = match rootOp with | Op (fst, snd, _) -> (fst,snd)

    let (calculated, remaining) = (calc 0 Map.empty sorted [])
    let remMap = remaining |> Map.ofList

    // 1. Find known and unknown side
    // 2. Perform opposite operation on the known value
    // 3. Repeat for unknown side until we only have 'humn' left
    let rec solve knownVal (rightSide:Operation) =
        match rightSide with
        | Number x -> 
            printfn "This should not happen!"
            2L
        | Op (l, r, op) -> 
            
            if calculated.ContainsKey r then
                let rightVal = calculated[r]
                let updatedVal = 
                    match op with
                    | '*' -> knownVal / rightVal
                    | '/' -> knownVal * rightVal
                    | '+' -> knownVal - rightVal
                    | '-' -> knownVal + rightVal

                if l = "humn" then
                    updatedVal
                else
                    solve updatedVal remMap[l]
            else
                let leftVal = calculated[l]
                let updatedVal = 
                    match op with
                    | '*' -> knownVal / leftVal
                    | '/' -> leftVal / knownVal
                    | '+' -> knownVal - leftVal
                    | '-' -> leftVal - knownVal

                if r = "humn" then
                    updatedVal
                else
                    solve updatedVal remMap[r]
        
    
    let knownPart = if calculated.ContainsKey (fst monkeys) then calculated[fst monkeys] else calculated[snd monkeys]
    let remPart = if calculated.ContainsKey (fst monkeys) then remMap[snd monkeys] else remMap[fst monkeys]

    solve knownPart remPart


let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "21" |> Async.RunSynchronously)
    Assert.Equal("309248622142100", part1)
    Assert.Equal("3757272361782", part2)
