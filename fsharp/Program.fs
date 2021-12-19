open System.Diagnostics
open InputProvider
open System

// Setup inspired from here: https://github.com/Spinkelben/adventofcode/blob/master/Program.fs
let private solvers year day =
    match year with
    | "2021" -> match day with
                | "1" -> Some _2021_1.execute
                | "2" -> Some _2021_2.execute
                | "3" -> Some _2021_3.execute
                | "4" -> Some _2021_4.execute
                | "5" -> Some _2021_5.execute
                | "6" -> Some _2021_6.execute
                | "7" -> Some _2021_7.execute
                | "8" -> Some _2021_8.execute
                | "9" -> Some _2021_9.execute
                | "10" -> Some _2021_10.execute
                | "11" -> Some _2021_11.execute
                | "12" -> Some _2021_12.execute
                | "13" -> Some _2021_13.execute
                | "14" -> Some _2021_14.execute
                | "15" -> Some _2021_15.execute
                | "16" -> Some _2021_16.execute
                | "17" -> Some _2021_17.execute
                | "18" -> Some _2021_18.execute
                | "19" -> Some _2021_19.execute
                | "20" -> Some _2021_20.execute
                | "21" -> Some _2021_21.execute
                | "22" -> Some _2021_22.execute
                | "23" -> Some _2021_23.execute
                | "24" -> Some _2021_24.execute
                | "25" -> Some _2021_25.execute
                | _ -> None
    | _      -> None

let printResult result =
    printfn "Solution:"
    printfn "  Part 1: %s \r\n  Part 2: %s" (fst result) (snd result)

[<EntryPoint>]
let main argv = 
    let year = if argv.Length > 0 then argv.[0] else "2021"
    let day = if argv.Length > 1 then argv.[1] else "18"

    printfn "## Puzzle %s/12-%s" day year
    printfn ""

    let solver = solvers year day
    let stopwatch = Stopwatch.StartNew();

    let input = getPuzzleInput year day |> Async.RunSynchronously

    match solver with 
        | Some s -> input |> s |> printResult
        | None -> printfn "No solver for specified puzzle"
    stopwatch.Stop()
    printfn ""
    printf "Time spent: %s" (stopwatch.Elapsed.ToString())

    0