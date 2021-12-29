module _2015_01

open System
open Xunit

// Task 1: ( = +1, ) = -1, whats the final number? 
// Task 2: At what index is the number minus for the first time (idx start at 1)

let parseInput (input : string seq) = 
    input |> Seq.head

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = parsed |> Seq.fold (fun agg cur ->
        match cur with
        | '(' -> agg + 1
        | ')' -> agg - 1) 0

    
    let part2 = 
        parsed 
        |> Seq.indexed 
        |> Seq.map (fun (idx, ch) -> (idx + 1, ch))
        |> Seq.fold (fun (idx, floor) (currentIdx, ch) ->
            if floor < 0 then
                (idx, floor)
            else
                match ch with
                | '(' -> (currentIdx, floor + 1)
                | ')' -> (currentIdx, floor - 1)) (0, 0)
        

    part1.ToString(), (fst part2).ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute []
    Assert.Equal("232",part1)
    Assert.Equal("1783",part2)