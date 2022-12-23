module _2022_11

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Collections.Generic

// Task 1: Simulate monkey throws and count each inspection
// Task 2: Do the same thing as before, but 10.000 times and without dividing by 3 each time.

let parseInput (input : string seq) = 
    let monkeys = 
        input 
        |> Seq.filter (fun x -> x <> "") 
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (x.StartsWith "Monkey "))
        |> Seq.chunkBySize 5 
        |> Seq.map (fun x -> 
            x[0] <- x[0].Substring 16
            x[1] <- x[1].Substring 17
            x[2] <- x[2].Substring 19
            x[3] <- x[3].Substring 25
            x[4] <- x[4].Substring 26
            x)
        |> Array.ofSeq

    let items = monkeys |> Array.map (fun x -> (x[0].Split ", ") |> Array.map int64 |> Queue<int64>) 
    let operations = monkeys |> Array.map (fun x -> x[1]) |> Array.map (fun x -> 
        let op = 
            match x[4] with 
            | '*' -> (*)
            | '+' -> (+)
        let funk = 
            match (x.Substring 6) with
            | "old" -> (fun x -> op x x)
            | rest -> (fun x -> op x (int64 rest))

        funk)
    let tests = monkeys |> Array.map (fun x -> int64 x[2]) |> Array.map (fun x -> (fun v -> (v % x) = 0L))

    let divisor = monkeys |> Array.map (fun x -> int64 x[2]) |> Array.fold (*) 1L

    let trues = monkeys |> Array.map (fun x -> int x[3])
    let falses = monkeys |> Array.map (fun x -> int x[4])
    let destinations = Array.zip trues falses

    (items, operations, tests, destinations, divisor)
    

let part1 ((items:Queue<int64> array), (operations:(int64 -> int64) array), (tests:(int64 -> bool) array), destinations:(int*int) array) =
    let inspectCounter = Array.zeroCreate (Array.length items)

    let rec processMonkeyThrows monkeyIdx : unit =
        if (items[monkeyIdx].Count = 0) then
            ()
        else
            // Inspect
            let mutable item = items[monkeyIdx].Dequeue()
            inspectCounter[monkeyIdx] <- inspectCounter[monkeyIdx] + 1
            item <- operations[monkeyIdx] item

            // Reduce worry
            item <- item / 3L

            // Throw
            let destination = 
                match tests[monkeyIdx] item with
                | true -> fst destinations[monkeyIdx]
                | false -> snd destinations[monkeyIdx]

            items[destination].Enqueue(item)

            processMonkeyThrows monkeyIdx

    [1 .. 20] |> Seq.iter (fun _ -> [0 .. items.Length-1] |> Seq.iter (fun x -> processMonkeyThrows x))
    
    let res = inspectCounter |> Array.sort |> Array.rev |> Array.take 2
    res[0] * res[1]

// The trick here is to realize that all the tests are "is divisible by x" where 'x' is all prime numbers.. Then you can multiply them all together
// and reduce the worry-level by taking the modulo.
let part2 ((items:Queue<int64> array), (operations:(int64 -> int64) array), (tests:(int64 -> bool) array), destinations:(int*int) array, divisor) =
    let inspectCounter = Array.zeroCreate (Array.length items)

    let rec processMonkeyThrows monkeyIdx : unit =
        if (items[monkeyIdx].Count = 0) then
            ()
        else
            // Inspect
            let mutable item = items[monkeyIdx].Dequeue()
            inspectCounter[monkeyIdx] <- inspectCounter[monkeyIdx] + 1
            item <- operations[monkeyIdx] item

            // Reduce worry
            item <- item % divisor

            // Throw
            let destination = 
                match tests[monkeyIdx] item with
                | true -> fst destinations[monkeyIdx]
                | false -> snd destinations[monkeyIdx]

            items[destination].Enqueue(item)

            processMonkeyThrows monkeyIdx

    [1 .. 10000] |> Seq.iter (fun _ -> [0 .. items.Length-1] |> Seq.iter (fun x -> processMonkeyThrows x))
    
    let res = inspectCounter |> Array.sort |> Array.rev |> Array.take 2
    (int64 res[0]) * (int64 res[1])
            

let execute (input : string seq) =
    //printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let (items, ops, tests, dests, divisor) = parsed
    let part1 = part1 (items, ops, tests, dests)

    let part2 = part2 (parseInput input)

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "11" |> Async.RunSynchronously)
    Assert.Equal("58794", part1)
    Assert.Equal("20151213744", part2)