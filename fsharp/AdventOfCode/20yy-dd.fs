﻿module _20yy_xx

open System
open Xunit

// Task 1: 
// Task 2: 

let parseInput (input : string seq) = 
    input

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = "N/A"

    let part2 = "N/A"

    part1.ToString(), part2.ToString()

//[<Fact>]
//let ``Test``() =
//    let (part1, part2) = execute []
//    Assert.Equal(1,1)