module _2023_04

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Linq

let parseInput (input : string seq) = 
    let notEmpty x = not (String.IsNullOrEmpty x)

    input
    |> Seq.map (fun x -> x.Split ": ")
    |> Seq.map (fun x -> x[1])
    |> Seq.map (fun x -> x.Split " | ")
    |> Seq.map (fun x -> x[0].Trim(), x[1].Trim())
    |> Seq.map (fun (x,y) -> 
        (x.Split " ") |> Array.filter notEmpty |> Array.map int, 
        (y.Split " ") |> Array.filter notEmpty |> Array.map int)
    |> List.ofSeq

let part1 input =
    input 
    |> List.map (fun (x,y) -> (Seq.ofArray x).Intersect(Seq.ofArray y) |> List.ofSeq)
    |> List.filter (fun x -> x.Length > 0)
    |> List.map (fun x -> x.Length)
    |> List.map (fun x -> Math.Pow(2.0, (float x) - 1.0))
    |> List.sum


let part2 input =
    let points = 
        input 
        |> List.map (fun (x,y) -> (Seq.ofArray x).Intersect(Seq.ofArray y) |> List.ofSeq)
        |> List.map (fun x -> x.Length)
    let scratchCards = Array.init points.Length (fun _ -> 1)

    // Since we can never win the first card and only the second card once etc, 
    // we just iterate from the top and add up the cards as we go
    for i in 0 .. (List.length points - 1) do
        for y in i+1 .. min (i + points[i]) (scratchCards.Length-1) do
            scratchCards[y] <- scratchCards[y] + scratchCards[i]

    scratchCards |> Array.sum

let execute (input : string seq) =
    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "4" |> Async.RunSynchronously)
    Assert.Equal("23028", part1)
    Assert.Equal("9236992", part2)