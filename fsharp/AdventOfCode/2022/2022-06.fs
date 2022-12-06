module _2022_6

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Find our how many characters in the string before we have 4 different chars in a row
// Task 2: Same as before but with 14 distinct chars

let parseInput (input : string seq) = 
    input |> Seq.exactlyOne

// Just hardcode all possibilities
let part1 input =
    let rec solve (count:int) (remaining:char list) =
        match remaining with
        | [] -> count
        | fst::snd::trd::frth::rest -> 
            if (fst <> snd && fst <> trd && fst <> frth &&
                snd <> trd && snd <> frth &&
                trd <> frth) then
                count + 1
            else
                solve (count + 1) (snd::trd::frth::rest)
    solve 3 (input |> List.ofSeq)

// Iterate through each char and compare it to the previous 14 chars. Keep track of 
// where you are and the last duplicate (e.g. if you idx 5 and 10 are dupes, you want to 
// remember idx 5). When there is 14 chars between the last dupe and your current idx you 
// are done
let part2 input = 
    let rec checkForDupe (currIdx:int) (lowIdx:int) (highIdx:int) (input:string) =
        if (lowIdx >= highIdx) then
            (false, -1)
        else if (input[currIdx] = input[highIdx]) then
            (true, highIdx)
        else
            checkForDupe currIdx lowIdx (highIdx - 1) input
    let rec solve (currIdx:int) (lastDupe:int) input =
        
        let (isDupe, dupeIdx) = checkForDupe currIdx (max 0 (currIdx - 13)) (currIdx - 1) input

        if (isDupe) then
            solve (currIdx + 1) (max dupeIdx lastDupe) input
        else if ((currIdx - 13) > lastDupe) then
            currIdx + 1
        else
            solve (currIdx + 1) lastDupe input
            
    solve 0 0 input

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()


[<Fact>]
let ``DebugTest``() =
    Assert.Equal(19, (part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"));
    Assert.Equal(23, (part2 "bvwbjplbgvbhsrlpgdmjqwftvncz"));
    Assert.Equal(23, (part2 "nppdvjthqldpwncqszvftbrmjlhg"));
    Assert.Equal(29, (part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"));
[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "6" |> Async.RunSynchronously)
    Assert.Equal("1987", part1)
    Assert.Equal("3059", part2)