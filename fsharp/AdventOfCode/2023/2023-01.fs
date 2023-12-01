module _2023_01

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Find first and last number, combine them and sum up all numbers
// Task 2: Numbers can be spelled out..

let part1 input =
    input
        |> Seq.map (fun x -> x |> Seq.find Char.IsNumber, x |> Seq.findBack Char.IsNumber)
        |> Seq.map (fun (x,y) -> (charToInt x) * 10 + charToInt y)
        |> Seq.sum

let rec findNumber (line:string) (numbers:string array) =
    let results = numbers |> Array.filter (fun x -> line.StartsWith x)

    match results with
    | [||] -> findNumber line[1..] numbers
    | _ -> results[0]

let parseNumber (number:string) =
    match number.Length, number with
    | (1, x) -> int32 x
    | (3, "one") -> 1 
    | (3, "two") -> 2 
    | (3, "six") -> 6
    | (4, "four") -> 4
    | (4, "five") -> 5
    | (4, "nine") -> 9
    | (5, "three") -> 3
    | (5, "seven") -> 7
    | (5, "eight") -> 8
    
let parseLine (line:string) =
    let numbers = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; 
                     "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]

    let reversedInput = line |> Seq.rev |> String.Concat
    let reversedNumbers = 
        numbers 
        |> Array.map Seq.rev
        |> Array.map String.Concat
        

    let firstNumber = findNumber line numbers |> parseNumber
    let secondNumber = findNumber reversedInput reversedNumbers |> Seq.rev |> String.Concat |> parseNumber

    firstNumber * 10 + secondNumber

let part2 input = input |> Seq.map parseLine |> Seq.sum
        

let execute (input : string seq) =

    let part1 = part1 input

    let part2 = part2 input

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "1" |> Async.RunSynchronously)
    Assert.Equal("55029", part1)
    Assert.Equal("55686", part2)