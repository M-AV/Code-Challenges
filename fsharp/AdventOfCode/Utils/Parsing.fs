module Parsing

let charToInt ch = (int ch) - (int '0')
let ints (input:string) = input |> Seq.map charToInt |> Array.ofSeq
let parseArray2d (input: string seq) =
    let arr = 
        input
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])