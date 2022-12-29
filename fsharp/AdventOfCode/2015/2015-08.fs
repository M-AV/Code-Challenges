module _2015_8

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Given a bunch of strings with escaped symbols, find out how long the string is. Then subtract that number 
//         the length incl. escape chars.
// Task 2: Re-escape the strings. Then take the new length and subtract the original length

let parseInput (input : string seq) = 
    input

// For each string:
// - Count '\\' and remove them from the string (in case of \\\ only the first 2 will be counted and removed)
// - Count '\x' and remove them from string (one \x counts as 3) - Here we don't risk counting '\\x' 
//   because we removed '\\' in previous check
// - Count '\"' and find result
let part1 (input : string seq) =
    let res =
        input
        |> Seq.map (fun x -> x, x.Length - 2, x.Length)
        |> Seq.map (fun (x, n, y) -> 
            let parts = x.Split("""\\""")
            String.Join("",parts), n-(parts.Length-1), y)
        |> Seq.map (fun (x, n, y) -> 
            printf "%s :" x
            let parts = x.Split "\\x"
            String.Join("",parts), n-((parts.Length-1) * 3), y)

        |> Seq.map (fun (x, n, y) -> 
            let parts = x.Split @"\"""
            x, n-(parts.Length-1), y)
        |> Seq.map (fun (x,n,y) -> y - n)
        |> Seq.sum

    res

// Simply add a '\' for each existing '\' first, then for each '"' and then count 2 extra quotes
let part2 (input : string seq)  =
    input 
    |> Seq.map (fun x -> x, x.Length)
    |> Seq.map (fun (x,l) -> x.Replace("\\", "\\\\").Replace("\"", "\\\"").Length + 2, l)
    |> Seq.map (fun (x,l) -> x - l)
    |> Seq.sum

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "8" |> Async.RunSynchronously)
    Assert.Equal("1350", part1)
    Assert.Equal("2085", part2)