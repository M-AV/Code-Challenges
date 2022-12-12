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


let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None