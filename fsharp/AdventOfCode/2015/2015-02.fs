module _2015_02

open System
open Xunit

// Task 1: Calc required wrapping paper
// Task 2: Calc required ribbon

let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> x.Split 'x')
    |> Seq.map (fun x -> (Int32.Parse x[0], Int32.Parse x[1], Int32.Parse x[2]))
    |> List.ofSeq

let wrappingRequired (l, w, h) =
    let side1 = l * w
    let side2 = w * h
    let side3 = h * l

    let extra = min (min side1 side2) side3
    2 * side1 + 2 * side2 + 2 * side3 + extra

let ribbonRequired (l, w, h) =
    let side1 = 2 * l + 2 * w
    let side2 = 2 * w + 2 * h
    let side3 = 2 * h + 2 * l

    (min (min side1 side2) side3) + l * w * h

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = parsed |> Seq.map wrappingRequired |> Seq.sum
    let part2 = parsed |> Seq.map ribbonRequired |> Seq.sum

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute []
    Assert.Equal("1588178",part1)
    Assert.Equal("3783758",part2)