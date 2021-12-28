module _2019_01

open System
open Xunit

// Task 1: Divide each number with 3 and subtract 2, then sum
// Task 2: 

let parseInput (input : string seq) = 
    input |> Seq.map Int32.Parse

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = 
        parsed
        |> Seq.map (fun x -> x / 3 - 2)
        |> Seq.sum

    let part2 = "N/A"

    part1.ToString(), part2.ToString()

//[<Fact>]
//let ``Test``() =
//    let (part1, part2) = execute []
//    Assert.Equal(1,1)