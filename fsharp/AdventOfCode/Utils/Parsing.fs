﻿module Parsing

open System

let trd (_, _, c) = c

let charToInt ch = (int ch) - (int '0')
let intToChar i = char (i + (int '0'))
let ints (input:string) = 
    input |> Seq.map charToInt |> Array.ofSeq
let parseArray2d (input: string seq) =
    let arr = 
        input
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])

/// Adds an extra row to top/botton and a column in beginning and end. 
/// The extra padding can eliminate boundary checks for some challenges, making the code simpler.
let parsePadded2dArray (paddingVal:char) (input:string seq) =
    let parsed = parseArray2d input
    let rows = Array2D.length1 parsed
    let cols = Array2D.length2 parsed
    let newArray = Array2D.init (rows + 2) (cols + 2) (fun x y -> paddingVal)

    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            newArray[i + 1,j + 1] <- parsed[i,j]
    
    newArray

// Logic is fairly simple, but I found this while searching for a built in way: https://stackoverflow.com/a/49891028/1401257
let find2d needle (arr: char[,]) =
    let rec search x y =
        if y >= arr.GetLength 1 then None
        else if x >= arr.GetLength 0 then search 0 (y+1)
        else if arr[x,y] = needle then Some(x,y)
        else search (x+1) y
    search 0 0

// Batching logic from here: https://stackoverflow.com/a/7518857/1401257
let batchesOf n = 
    Seq.mapi (fun i v -> i / n, v) >>
    Seq.groupBy fst >>
    Seq.map snd >>
    Seq.map (Seq.map snd)

let batchesOf2 input =
    input
    |> Seq.mapi (fun i v -> i / 2, v)
    |> Seq.groupBy fst 
    |> Seq.map snd
    |> Seq.map (Seq.map snd)
    |> Seq.map (Array.ofSeq)
    |> Seq.map (fun x -> x[0],x[1])

let printGrid2d grid =
    for y = 0 to (Array2D.length2 grid)-1 do 
        let s = String(grid[0..(Array2D.length1 grid)-1, y])
        printfn "%s" s
    printfn ""

let printGrid2d_int grid =
    for y = 0 to (Array2D.length2 grid)-1 do 
        for x = 0 to (Array2D.length1 grid)-1 do
            let s = (int grid[x,y]).ToString()
            printf "%s" s
        printfn ""
    printfn ""


let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None
