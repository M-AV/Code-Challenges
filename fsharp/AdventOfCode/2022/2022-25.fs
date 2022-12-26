module _2022_25

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Parse SNAFU numbers, sum them up and then present the sum as a SNAFU number

let parseInput (input : string seq) = 
    input |> List.ofSeq

let parseSNAFU (number:string) =
    let rec parse idx (idxVal:int64) (res:int64) =
        if idx = -1 then res
        else
            match number[idx] with
            | '0' -> parse (idx-1) (idxVal * 5L) res
            | '1' -> parse (idx-1) (idxVal * 5L) (res + (int64 idxVal))
            | '2' -> parse (idx-1) (idxVal * 5L) (res + (int64 idxVal)*2L)
            | '-' -> parse (idx-1) (idxVal * 5L) (res - (int64 idxVal))
            | '=' -> parse (idx-1) (idxVal * 5L) (res - (int64 idxVal)*2L)


    parse (number.Length-1) 1L 0L

let intToSNAFU (number:int64) =
    let rec findLength (idx:int) =
        let nextIdx = idx+1
        let base5 = int64 (Math.Pow(5, nextIdx))
        if (number <= (base5 / 2L)) then
            idx
        else
            findLength nextIdx

    let rec solve (idx:int) (remaining:int64) answer =
        let base5 = int64 (Math.Pow(5, idx))
        let f = solve (idx-1)
        let nextPow = (int64 (Math.Pow(5, idx))) / 2L
        if (idx < 0) then
            answer
        elif remaining = 0 then
            f remaining (answer+"0")
        elif remaining < 0 then
            if (abs remaining) <= nextPow then
                f remaining (answer+"0")
            elif (abs remaining) <= (base5 + (base5 / 2L)) then
                f (remaining+base5) (answer+"-")
            else
                f (remaining+base5*2L) (answer+"=")
        else
            if remaining <= nextPow then
                f remaining (answer+"0")
            elif remaining <= (base5 + (base5 / 2L)) then
                f (remaining-base5) (answer+"1")
            else
                f (remaining-base5*2L) (answer+"2")
        
    let length = findLength 0
    solve length number ""

let part1 input =
    let sum = 
        input 
        |> List.map parseSNAFU
        |> List.sum

    intToSNAFU sum

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = "Merry Christmas"

    part1.ToString(), part2.ToString()

[<Fact>]
let ``SnafuTest``() =
    Assert.Equal("=2", intToSNAFU(-3))
    Assert.Equal("1", intToSNAFU(1))
    Assert.Equal("2", intToSNAFU(2))
    Assert.Equal("1=", intToSNAFU(3))
    Assert.Equal("1-", intToSNAFU(4))
    Assert.Equal("10", intToSNAFU(5))
    Assert.Equal("11", intToSNAFU(6))
    Assert.Equal("12", intToSNAFU(7))
    Assert.Equal("2=", intToSNAFU(8))
    Assert.Equal("2-", intToSNAFU(9))
    Assert.Equal("20", intToSNAFU(10))
    Assert.Equal("22", intToSNAFU(12))
    Assert.Equal("1=0", intToSNAFU(15))
    Assert.Equal("1-0", intToSNAFU(20))
    Assert.Equal("1=11-2", intToSNAFU(2022))
    Assert.Equal("1-0---0", intToSNAFU(12345))
    Assert.Equal("1121-1110-1=0", intToSNAFU(314159265))

[<Fact>]
let ``Test``() =
    let (part1, _) = execute (getPuzzleInput "2022" "25" |> Async.RunSynchronously)
    Assert.Equal("2=0-2-1-0=20-01-2-20", part1)
