module _2015_04

open InputProvider
open System
open Xunit
open System.Security.Cryptography
open System.Text

// Task 1: Find number postfix for input that would result in an MD5 hash that starts with 00000
// Task 2: Same but target is 000000 instead

let parseInput (input : string seq) = 
    input |> Seq.head

let md5 = MD5.Create()
let rec findMd5Hash (desiredPrefix:string) input count =
    let bytes = input + count.ToString() |> Encoding.UTF8.GetBytes
    let b = md5.ComputeHash bytes
    let hash = md5.ComputeHash bytes |> BitConverter.ToString
    if hash.StartsWith desiredPrefix then
        count
    else
        findMd5Hash desiredPrefix input (count + 1)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = findMd5Hash "00-00-0" parsed 1

    let part2 = findMd5Hash "00-00-00" parsed part1

    part1.ToString(), part2.ToString()

let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2015" "4" |> Async.RunSynchronously)
    Assert.Equal("117946", part1)
    Assert.Equal("3938038", part2)