module _2015_7

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Input is a series of equations that basically add up to one big equation. Find the value of 'a'
// Task 2: Take the previously calculated value of 'a' and set 'b' equal to that. Find the new value of 'a'

type Op =
    | Bin of string * string * string
    | Un of string * string
    | Val of int
    | Var of string

// Parse input into a map of variables and their corresponding 'equation'
// Each 'formula' will be parsed into the above discriminated union, allowing me 
// to easily identify the variables I need and the operation that needs to be performed.
let parseInput (input : string seq) = 
    input 
    |> Seq.map (fun x -> 
        let split = x.Split " -> "
        let target = split[1]

        match Int32.TryParse split[0] with
        | true, int -> target, Val(int)
        | _ ->
            let parts = split[0].Split ' '
            if parts.Length = 3 then
                target, Bin(parts[0], parts[1], parts[2])
            elif parts.Length = 2 then
                target, Un(parts[0], parts[1])
            else
                target, Var(parts[0])
    )
    |> Map.ofSeq

// To find the variable 'a', we recursively calculate each required variable, until we have everything we need to find 'a'
// 1. If variable is an integer, just return the value
// 2. If variable is already known, return known value
// 3. Recursively calculate value of required variables and perform operation
// 4. Update the map of known values before we return
let rec getVal (input:Map<string,Op>) =
    let rec calc (parsed:Map<string, int>) (register:string) =
        match Int32.TryParse register with
        | true, v -> v, parsed
        | _ ->
            if parsed.ContainsKey register then
                parsed[register], parsed
            else
                match input[register] with
                | Val i ->  
                    i, (parsed |> Map.add register i)
                | Un (cmd, var) -> 
                    let sndVal, mapped = calc parsed var
                    let res = 
                        match cmd with
                        | "NOT" -> ~~~sndVal
                    res, (mapped |> Map.add register res)
                | Bin (fst, cmd, snd) -> 
                    let fstVal, mapped = calc parsed fst
                    let sndVal, mapped2 = calc mapped snd
                    let res = 
                        match cmd with
                        | "AND" -> fstVal &&& sndVal
                        | "OR" -> fstVal ||| sndVal
                        | "LSHIFT" -> fstVal <<< sndVal
                        | "RSHIFT" -> fstVal >>> sndVal
                    res, (mapped2 |> Map.add register res)
                | Var v ->
                    let res, mapped = calc parsed v
                    res, (mapped |> Map.add register res)

    let (aVal, mapped) = calc Map.empty "a"
    aVal

let part1 (input:Map<string, Op>) =
    getVal input

let part2 input =
    let part1Res = getVal input

    let modifiedInput = input |> Map.change "b" (fun x -> Some(Val(part1Res)))

    part1 modifiedInput

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "7" |> Async.RunSynchronously)
    Assert.Equal("956", part1)
    Assert.Equal("40149", part2)