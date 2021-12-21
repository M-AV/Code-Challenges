module _2021_20

// Task 1: For each group of 9 elements in the input, sum and match with the reference array. Count how many #'s after 2 iterations
// Task 2: Same same, but 50 iterations

type Image = int * int[,]

let cToB x = if x = '#' then 1 else 0
let parseInput (input: string seq) =
    let referencePixels = 
        input
        |> Seq.head
        |> Seq.map cToB
        |> Array.ofSeq
    let image = 
        input
        |> Seq.tail
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    let multiArr = Array2D.init (image[0].Length) image.Length (fun x y -> cToB (image[y][x]))
    (referencePixels, Image(0, multiArr))

let convertBitsToInt (bits:int list) =
    let mutable res = 0
    for i in bits do
        res <- res <<< 1
        res <- res ||| i
    res
let convertBitsToInt64 (bits:int list) =
    let mutable res = 0L
    for i in bits do
        res <- res <<< 1
        res <- res ||| i
    res

let transformImage (reference:int array) ((infi, image):Image) =
    let mapValue x y = 
        let l x y =
            if x < 0 || y < 0 then
                infi
            else if x >= Array2D.length2 image then
                infi
            else if y >= Array2D.length1 image then
                infi
            else
                image[x, y]
        let res = [
            l (x-2) (y-2); l (x - 1) (y - 2); l x (y - 2);
            l (x-2) (y-1); l (x - 1) (y - 1); l x (y - 1);
            l (x-2) (y)  ; l (x - 1) y      ; l x y      
        ]

        let temp = reference[convertBitsToInt res]

        //printfn "X: %i, Y: %i" x y
        //printfn "%A" (res |> List.take 3)
        //printfn "%A" (res |> List.skip 3 |> List.take 3)
        //printfn "%A    idx: %i    val: %i" (res |> List.skip 6 |> List.take 3) (convertBitsToInt res) (reference[convertBitsToInt res])
        
        reference[convertBitsToInt res]
       
    let newInfinite = if infi = 0 then reference[0] else reference[511]

    let transformed = Array2D.init ((Array2D.length2 image) + 2) ((Array2D.length1 image) + 2) (fun x y -> mapValue x y)
    Image(newInfinite, transformed)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let (reference, image) = parseInput input

    let transformed = transformImage reference image
    let transformed2 = transformImage reference transformed

    // I spent a lot of time debugging this only to realize I was using the wrong array when counting 1's 
    // and 2D arrays are printed mirrored when using %A... 
    //printfn "%A" image
    //printfn "%A" transformed
    //printfn "%A" transformed2

    let getAllElements (a:int[,]) : int seq =
        seq { for i in 0 .. a.GetLength(0)-1 do
              for j in 0 .. a.GetLength(1)-1 do yield a.[i,j] }

    let part1 = (snd transformed2) |> getAllElements |> Seq.filter (fun x -> x = 1) |> Seq.length

    let mutable res = transformed2
    for i in 1 .. 48 do
        res <- transformImage reference res

    let part2 = (snd res) |> getAllElements |> Seq.filter (fun x -> x = 1) |> Seq.length

    part1.ToString(), part2.ToString()