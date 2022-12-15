open System.Diagnostics
open InputProvider
open System
open Setup

// Setup inspired from here: https://github.com/Spinkelben/adventofcode/blob/master/Program.fs
let private solvers year day =
    match (year, day) with
    | ("2015", "1") -> Some _2015_01.execute
    | ("2015", "2") -> Some _2015_02.execute
    | ("2015", "3") -> Some _2015_03.execute
    | ("2015", "4") -> Some _2015_04.execute
    | ("2015", "5") -> Some _2015_05.execute

    | ("2016", "1") -> Some _2016_01.execute

    | ("2017", "1") -> Some _2017_01.execute

    | ("2018", "1") -> Some _2018_01.execute

    | ("2019", "1") -> Some _2019_01.execute

    | ("2020", "1") -> Some _2020_01.execute

    | ("2021", "1" )-> Some _2021_1.execute
    | ("2021", "2" )-> Some _2021_2.execute
    | ("2021", "3" )-> Some _2021_3.execute
    | ("2021", "4" )-> Some _2021_4.execute
    | ("2021", "5" )-> Some _2021_5.execute
    | ("2021", "6" )-> Some _2021_6.execute
    | ("2021", "7" )-> Some _2021_7.execute
    | ("2021", "8" )-> Some _2021_8.execute
    | ("2021", "9" )-> Some _2021_9.execute
    | ("2021", "10") -> Some _2021_10.execute
    | ("2021", "11") -> Some _2021_11.execute
    | ("2021", "12") -> Some _2021_12.execute
    | ("2021", "13") -> Some _2021_13.execute
    | ("2021", "14") -> Some _2021_14.execute
    | ("2021", "15") -> Some _2021_15.execute
    | ("2021", "16") -> Some _2021_16.execute
    | ("2021", "17") -> Some _2021_17.execute
    | ("2021", "18") -> Some _2021_18.execute
    | ("2021", "19") -> Some _2021_19.execute
    | ("2021", "20") -> Some _2021_20.execute
    | ("2021", "21") -> Some _2021_21.execute
    | ("2021", "22") -> Some _2021_22.execute
    | ("2021", "23") -> Some _2021_23.execute
    | ("2021", "24") -> Some _2021_24.execute
    | ("2021", "25") -> Some _2021_25.execute

    | ("2022", "1") -> Some _2022_1.execute
    | ("2022", "2") -> Some _2022_2.execute
    | ("2022", "3") -> Some _2022_3.execute
    | ("2022", "4") -> Some _2022_4.execute
    | ("2022", "5") -> Some _2022_5.execute
    | ("2022", "6") -> Some _2022_6.execute
    | ("2022", "7") -> Some _2022_7.execute
    | ("2022", "8") -> Some _2022_8.execute
    | ("2022", "9") -> Some _2022_9.execute
    | ("2022", "10") -> Some _2022_10.execute
    | ("2022", "11") -> Some _2022_11.execute
    | ("2022", "12") -> Some _2022_12.execute
    | ("2022", "13") -> Some _2022_13.execute
    | ("2022", "14") -> Some _2022_14.execute
    | ("2022", "15") -> Some _2022_15.execute
    | ("2022", "16") -> Some _2022_16.execute
    | ("2022", "17") -> Some _2022_17.execute
    | ("2022", "18") -> Some _2022_18.execute
    | ("2022", "19") -> Some _2022_19.execute
    | ("2022", "20") -> Some _2022_20.execute
    | ("2022", "21") -> Some _2022_21.execute
    | ("2022", "22") -> Some _2022_22.execute
    | ("2022", "23") -> Some _2022_23.execute
    | ("2022", "24") -> Some _2022_24.execute
    | ("2022", "25") -> Some _2022_25.execute
    | _ -> None

let printResult result =
    printfn "Solution:"
    printfn "  Part 1: %s \r\n  Part 2: %s" (fst result) (snd result)

let solvePuzzle (argv:string array) =
    let year = if argv.Length > 0 then argv.[0] else "2022"
    let day = if argv.Length > 1 then argv.[1] else "15"

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

[<EntryPoint>]
let main argv = 
    //Setup.setupYear()
    solvePuzzle argv

    
