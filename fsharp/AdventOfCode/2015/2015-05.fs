module _2015_05

open InputProvider
open System
open Xunit

// Task 1: Find nice strings
// Task 2: 

let parseInput (input : string seq) = 
    input

let isNice_old (line:string) =
    let vowels = ['a';'e';'i';'o';'u'] |> Set.ofList
    let vowelCount = line |> Seq.filter (fun x -> vowels.Contains x) |> Seq.length
    let hasTwiceInARow = 
        line 
        |> Seq.skip 1
        |> Seq.fold (fun (found, prev) cur -> 
            let same = prev = cur
            (found || same, cur)) (false, line |> Seq.head)
    let hasIllegalSubstring =
        line.Contains "ab" || 
        line.Contains "cd" || 
        line.Contains "pq" || 
        line.Contains "xy"
    (not hasIllegalSubstring) && vowelCount >= 3 && (fst hasTwiceInARow)

let isNice_new (line:string) =  
    let pairs =
        [0 .. line.Length - 2]
        |> Seq.filter (fun x -> line[x+2..].Contains (String.Concat([| line[x]; line[x+1] |])))
        |> Seq.length

    let repeatedLetters =
        [0 .. line.Length - 3]
        |> Seq.filter (fun x -> line[x] = line[x+2])
        |> Seq.length
    pairs >= 1 && repeatedLetters >= 1

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = parsed |> Seq.filter isNice_old |> Seq.length

    let part2 = parsed |> Seq.filter isNice_new |> Seq.length

    part1.ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "5" |> Async.RunSynchronously)
    Assert.Equal("236", part1)
    Assert.Equal("N/A", part2)