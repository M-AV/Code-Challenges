module _2022_20

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Move numbers in original order, as many spots as the number say and find the 1000th, 2000th and 3000th number after the 0. Add them up
// Task 2: Multiply all numbers by 811589153 and do 10 iterations of moves instead of just 1. 

let parseInput (input : string seq) = 
    input |> Seq.map int64 |> Array.ofSeq

// Challenge here is to keep track of the original order and their current position in the array
// To do that, when I move items around I maintain an array if indexes that map the original index to the current index (idxMap)
// I also make sure I can go the other way, so each item in the list (copy) will remember their original index.
// 
// To optimize a bit, I exploit the fact that any item being moved input.Length-1 steps will end up in the same spot 
// and only move the remainder (Moving an item to index 0 is the same as moving an item to the last index)
// 
// Takes around 1.5 seconds to run (Debug) or 0.6 (Release)
let solve (input:int64 array) (multiplier:int64) iterations =
    let idxMap = input |> Array.indexed |> Array.map fst

    // Originally I just capped the numbers here, because I thought I didn't need them. Somehow I managed to get the correct result for part 1 anyway. 
    // When starting part 2, I quickly realized this is broken and moved the capping down into the calculations below.
    let orgZero = Array.IndexOf(input, 0L)
    let copy = input |> Array.map (fun x -> x * multiplier) (*|> Array.map (fun x -> x % (int64 input.Length-1L))*) |> Array.indexed 

    let subFromIdx idx (v:int64) = 
        let idx64 = int64 idx
        if (idx64 + v) < 0 then 
            int(int64(copy.Length - 1) + idx64 + v) 
        else 
            int(idx64 + v)
    let addToIdx idx (v:int64) = int ((idx + v) % (int64 copy.Length - 1L))
    let nextIdx idx = (idx + 1) % (copy.Length)
    let prevIdx idx = if idx = 0 then copy.Length - 1 else idx - 1

    let goBack idx newIdx =
        let mutable i = idx
        let item = copy[idx]
        while i <> newIdx do
            copy[i] <- copy[prevIdx i]
            idxMap[fst copy[i]] <- i
            i <- prevIdx i

        // Move item
        copy[newIdx] <- item
        idxMap[fst item] <- newIdx

    let goForward idx newIdx =
        let mutable i = idx
        let item = copy[idx]
        while i <> newIdx do
            copy[i] <- copy[nextIdx i]
            idxMap[fst copy[i]] <- i
            i <- nextIdx i

        // Move item
        copy[newIdx] <- item
        idxMap[fst item] <- newIdx

    for j = 1 to iterations do
        for i = 0 to (idxMap.Length)-1 do
            //printfn "%A" (copy |> Array.map snd)
            let currentIdx = idxMap[i]
            let (_, item) = copy[currentIdx]
            let reduced = item % (int64(input.Length - 1)) // If an item moves input.Length-1 times it ends up in the same spot. So we cap it
            if (reduced = copy.Length) then
                copy[0] <- copy[0] // Noop - It would go all the way around
            else if reduced > 0 then
                let mutable i = currentIdx
                let newIdx = addToIdx currentIdx reduced

                if newIdx < currentIdx then // If we wrap around, go back instead of forward
                    goBack currentIdx newIdx
                else 
                    goForward currentIdx newIdx

            else if reduced < 0 then
                let mutable i = currentIdx
                let newIdx = subFromIdx currentIdx reduced

                if newIdx < currentIdx then
                    goBack currentIdx newIdx
                else 
                    goForward currentIdx newIdx


    //printfn "%A" (copy |> Array.map snd)
    
    let zeroIdx = idxMap[orgZero]

    //printfn "%A" copy[(zeroIdx + 1000) % copy.Length]
    //printfn "%A" copy[(zeroIdx + 2000) % copy.Length]
    //printfn "%A" copy[(zeroIdx + 3000) % copy.Length]

    (snd copy[(zeroIdx + 1000) % copy.Length]) + (snd copy[(zeroIdx + 2000) % copy.Length]) + (snd copy[(zeroIdx + 3000) % copy.Length])

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = solve parsed 1 1

    let part2 = solve parsed 811589153L 10

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "20" |> Async.RunSynchronously)
    Assert.Equal("11073", part1)
    Assert.Equal("11102539613040", part2)