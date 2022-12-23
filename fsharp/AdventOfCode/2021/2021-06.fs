module _2021_6

open InputProvider
open Xunit

// Task 1: Count multiplying fish after 80 days
// Task 2: Count them after 256 days (exponential so naive solution wont work)

let parseInitialState (line:string) =
    line.Split(',') |> Array.map (fun x -> int x)

let rec calcStateAfterDays state days : (int * int64) list=
    let processGroup state : (int * int64) list = 
        match state with 
        | (0, count) -> [ (6, count); (8, count) ]
        | (i, count) -> [ (i - 1, count) ]

    match days with 
    | 0 -> state
    | x -> calcStateAfterDays 
                // Here we just re-group to merge duplicate groups. We could probably have made a solution that
                // just updated the existing group. But whatevs.. This is fast enough and it works
                (state |> 
                    List.map processGroup |> 
                    List.collect id |>
                    List.groupBy fst |> 
                    List.map (fun (key, items) -> (key, items |> List.sumBy snd))) 
                (days - 1)     

    
let execute (input : string seq) =
    let initialState : (int * int64) list = 
        parseInitialState (input |> Seq.head) |> 
        List.ofArray |>
        List.countBy id |>
        List.map (fun (day, count) -> (day, int64 count))

    //printfn "%A" initialState

    printfn "Initial states: %i" (List.length initialState)

    let part1 = calcStateAfterDays initialState 80 |> List.map snd |> List.sum
    let part2 = calcStateAfterDays initialState 256 |> List.map snd |> List.sum

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2021" "6" |> Async.RunSynchronously)
    Assert.Equal("365862", part1)
    Assert.Equal("1653250886439", part2)