module _2015_10

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Text

// Task 1: Calculate length of Look-and-Say sequence after 40 iterations
// Task 2: Same, but 50

let parseInput (input : string seq) = 
    input |> Seq.exactlyOne

// For performance we access the string by idx only and avoid using substrings for the recursive calls
// We also use a StringBuilder for building the result
let rec iterate (result:StringBuilder) idx (source:string) =
    if idx >= source.Length then
        result
    else
        let mutable s = 1
        let mutable idx2 = idx + 1
        let mutable found = false
        while not found && idx2 < source.Length do
            if source[idx2] = source[idx] then
                s <- s+1
                idx2 <- idx2+1
            else
                found <- true

        iterate (result.Append(intToChar s).Append(source[idx])) (idx+s) source

        // Initial solution which I got part 1 with, but it takes something like a minute and is waay too slow for part 2.
        // let part = source |> Seq.skip idx |> Seq.takeWhile (fun x -> x = source[idx]) |> Seq.length
        // iterate (source[idx]::(intToChar part)::result) (idx+part) source

let solve input count =
    let mutable (str:string) = input

    for i = 1 to count do
        let sb = new StringBuilder((str.Length * 2))
        str <- (iterate sb 0 str).ToString()
        
    str.Length, str

let execute (input : string seq) =
    let parsed = parseInput input
    printfn "%A" parsed

    let (part1, strAfterPart1) = solve parsed 40
    let (part2, _) = solve strAfterPart1 10 // To avoid unnecessary iterations, we just continue where we left of from part 1

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "10" |> Async.RunSynchronously)
    Assert.Equal("360154", part1)
    Assert.Equal("5103798", part2)