module _2021_14

open Xunit
open System.Collections.Generic

// Task 1: Insert letters based on map for 10 iterations
// Task 2: Same Same, but 40

let parseInput (input: string seq) =
    let code = input |> Seq.head |> List.ofSeq
    let map = 
        input 
        |> Seq.skip 1
        |> Seq.map (fun x -> x.Split(" -> "))
        |> Seq.map (fun x -> ((x[0][0], x[0][1]), x[1][0]))
        |> Map.ofSeq

    (map, code)

let performIterations iterations (code:char list) (map:Map<char*char, char>) =
    // A recursive solution was quite easy. Had to think a bit to make sure it was tail-recursive
    // since the input otherwise causes a StackOverflowException (even for 10 iterations)
    let rec iterate result (code:char list) : char list=
        match code with
        | [] -> result
        | head::tail when tail.IsEmpty -> head :: result
        | head::tail when tail.Length > 0 ->
            let toInsert = 
                match map.TryFind (head, tail.Head) with
                | Some(x) -> [ x ]
                | None -> []

            iterate (toInsert @ head :: result) tail
        | _ -> result

    let mutable result = code;
    for i in 1 .. iterations do
        result <- (iterate [] result) |> List.rev

    result

// I FELL IN THE TRAP!! :(
// Part 2 requires too many iterations for the above function to be viable.. Should have realized 
// earlier I needed a smarter solution.. Oh well.
// Below is solution for Part 2:
// The idea here is instead of actually calculating the string for each iteration,
// we just keep a count of all the pairs we have and use those to calculate how many
// new pairs we get
// E.g. if we have AB 3 times and rule AB -> C, we will after 1 iteration have 3 AC 
//      and 3 CB.
// I'm sure this can be done in a simpler way..

let rec countPairs code result =
    match code with 
    | [] -> result
    | head :: [] -> result
    | head :: tail -> 
        let temp =
            result |> Map.change (head, tail.Head) (fun x ->
                match x with 
                | Some v -> Some(v + 1L)
                | None -> Some(1L))
        countPairs tail temp
let countCharsAfterIterations iterations (code:char list) (map:Map<char*char, char>) =
    let getNewPairs pair (map:Map<char*char, char>) : (char * char)[] =
        match map.TryFind pair with
        | Some x -> [| (fst pair, x); (x, snd pair) |]
        | None -> [||]
    let updateMap incr x =
        match x with
        | Some cnt -> Some (cnt + incr)
        | _ -> Some incr

    let rec iterate (pairs:KeyValuePair<char*char, int64> list) (generatedPairs:Map<char * char, int64>) = 
        match pairs with
        | [] -> generatedPairs
        | head :: tail -> 
            match (getNewPairs head.Key map) with
            | [| x; y |] -> 
                let mutable newPairs = generatedPairs |> Map.change x (updateMap head.Value)
                newPairs <- newPairs |> Map.change y (updateMap head.Value)
                    
                iterate tail newPairs
            | _ -> generatedPairs |> Map.change head.Key (updateMap head.Value)

    let mutable result = countPairs code Map.empty;
    for _ in 1 .. iterations do
        result <- iterate (result |> List.ofSeq) Map.empty

    // Count first letter of each pair and add up counts (as last letter will be first in seperate map)
    // and add 1 extra for the last letter in the string which will always be last and therefore never first in a map
    let lastLetter = code |> List.rev |> List.head
    result |>
        Map.fold (fun (agg:Map<char, int64>) key value -> 
            agg |> Map.change (fst key) (fun x -> 
                match x with 
                | Some v -> Some(v + value)
                | _ -> Some(value))
            ) (Map [ (lastLetter, 1) ])
        

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let (map, code)= parseInput input
    //printfn "%A" (map |> List.ofSeq)
    //printfn "%A" code

    let counts = 
        (performIterations 10 code map)
        |> List.countBy id
    let max = counts |> Seq.maxBy snd
    let min = counts |> Seq.minBy snd

    let part1 = (snd max) - (snd min)

    let p2Counts = (countCharsAfterIterations 40 code map)
    let max2 = p2Counts |> Seq.map (fun x -> x.Value) |> Seq.max
    let min2 = p2Counts |> Seq.map (fun x -> x.Value) |> Seq.min

    printfn "Max: %i Min: %i" max2 min2

    let part2 = max2 - min2

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Example``() =
    let input = [ 
        "NNCB";
        "CH -> B";
        "HH -> N";
        "CB -> H";
        "NH -> C";
        "HB -> C";
        "HC -> B";
        "HN -> C";
        "NN -> C";
        "BH -> H";
        "NC -> B";
        "NB -> B";
        "BN -> B";
        "BB -> N";
        "BC -> B";
        "CC -> N";
        "CN -> C"; ]
    let (part1, part2) = execute input

    Assert.Equal("1588", part1);
    Assert.Equal("2188189693529", part2);