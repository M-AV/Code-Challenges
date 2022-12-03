module Parsing

let trd (_, _, c) = c

let charToInt ch = (int ch) - (int '0')
let ints (input:string) = 
    input |> Seq.map charToInt |> Array.ofSeq
let parseArray2d (input: string seq) =
    let arr = 
        input
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])

// Batching logic from here: https://stackoverflow.com/a/7518857/1401257
let batchesOf n = 
    Seq.mapi (fun i v -> i / n, v) >>
    Seq.groupBy fst >>
    Seq.map snd >>
    Seq.map (Seq.map snd)
