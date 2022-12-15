module _2022_15

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Each line represents a sensor and the location of the closest beacon (Manhattan distance). On line 2000000, how many positions cannot contain beacons
// Task 2: Within a 4000000 * 4000000 grid, find a beacon not detected by any sensor and use this function to get the result: x*4000000+y

let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> x.Substring 12)
    |> Seq.map (fun x -> 
        let idx = x.IndexOf ','
        int x[0..idx-1], x.Substring (idx+4))
    |> Seq.map (fun (x, rest) -> 
        let idx = rest.IndexOf ':'
        (x, int rest[0..idx-1]), rest.Substring (idx+25))
    |> Seq.map (fun (s, rest) ->
        let idx = rest.IndexOf ','
        let xB = int rest[0..idx-1]
        let yB = int rest[idx+4..rest.Length-1]
        s, (xB,yB), manhattanDistance2D s (xB,yB))
    |> List.ofSeq

let filterForRow row input =
    input
    |> Seq.filter (fun (s,b,d) -> 
        let distance = d
        let distanceToRow = manhattanDistance2D s (fst s, row)
        distance >= distanceToRow
        )
    |> Seq.map (fun (s,b,d) ->
        let distance = d
        let distanceToRow = manhattanDistance2D s (fst s, row)

        let numberOnEachSide = distance - distanceToRow
        fst s - numberOnEachSide, fst s + numberOnEachSide
        )

let part1 (input:((int*int)*(int*int)*int) seq) =
    let row = 2000000
    let relevant = filterForRow row input

    printfn "%A" (relevant |> List.ofSeq)

    let beaconsOnRow = input |> Seq.map (fun (a,b,c) -> b) |> Seq.filter (fun (x,y) -> y = row) |> Seq.distinct |> Seq.length
    let sensorsOnRow = input |> Seq.map (fun (a,b,c) -> a) |> Seq.filter (fun (x,y) -> y = row) |> Seq.length

    let s = 
        relevant 
        |> Seq.sortBy fst 
        |> Seq.fold (fun (sum, (x,y)) (x', y') -> 
            if (x' > y) then
                sum + (y'-x' + 1), (x,y')
            elif (y' <= y) then
                sum, (x,y)
            else
                sum + (y'-y), (x, max y y')
            ) (0, (Int32.MinValue, Int32.MinValue))

    fst s - beaconsOnRow - sensorsOnRow


// This is slow (takes about ~9 seconds on my PC in Debug mode), but it works, so I'll leave it as is.
let part2 input =
    let limit = 4000000
    let searchRow rowIdx =
        let relevant = filterForRow rowIdx input

        let s = 
            relevant 
            |> Seq.map (fun (x,y) -> max x 0,max y 0)
            |> Seq.filter (fun (x,y) -> x <= limit || y <= limit)
            |> Seq.map (fun (x,y) -> min limit x, min limit y)
            |> Seq.sortBy fst 
            |> Seq.fold (fun (sum, (x,y)) (x', y') -> 
                if (x' > y) then
                    sum + (y'-x' + 1), (x,y')
                elif (y' <= y) then
                    sum, (x,y)
                else
                    sum + (y'-y), (x, max y y')
                ) (0, (Int32.MinValue, Int32.MinValue))
        fst s

    let row = seq { 0 .. limit } |> Seq.find (fun x -> 
        let s = (searchRow x) 
        s < limit+1);

    let column = seq { 0 .. limit } |> Seq.find (fun x -> 
        input |> Seq.forall (fun (s, b, d) -> 
            let dist = manhattanDistance2D (x,row) s
            let bDist = d
            dist > bDist)
        )

//  printfn "%A %A" row column;

    int64 column * 4000000L + int64 row

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "15" |> Async.RunSynchronously)
    Assert.Equal("5040643", part1)
    Assert.Equal("11016575214126", part2)