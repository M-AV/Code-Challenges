module _2021_3

open System

// Task 1: Merge bits by most common bit in a given position and calculate resulting int value and negated value (you don't have 32 bits)
// Task 2: 

let tap data = 
  let materialized = List.ofSeq data
  materialized

// Go through each bit and left shift the appropriate amount to get an integer
let bitsToInt bits length =
    let (x, _, _) =
        bits |>
        Seq.fold (fun (value, cnt, length) curr -> (value ||| (curr <<< (length - cnt - 1)), cnt + 1, length)) (0, 0, length)
    x
let valAt values position = 
    let item = values |> Seq.indexed |> Seq.find (fun x -> fst x = position)
    snd item
// Were using position from the "wrong" side because thats more convenient for this task
let mostCommonBit items position =
    let calc = 
        items |> 
        Seq.map (fun s -> s |> Seq.map (fun c -> if c = 0 then -1 else c)) |> // Map bits to 1 and -1 - Easier for counting
        Seq.map (fun x -> 
                x |> 
                Seq.indexed |> 
                Seq.filter (fun x -> (fst x) = position) |> // Take item at right position based on index
                Seq.map (fun x -> snd x) |>                 // Remove index value
                Seq.head) |>                                // Take first (we should only have one anyway)
        Seq.fold (fun acc curr -> acc + curr) 0             // Add all items together 
    if calc >= 0 then 1 else 0                               // Map to 1 or 0
let leastCommonBit items position =
    let mostCommon = mostCommonBit items position
    if mostCommon = 1 then 0 else 1


let calcNextOxygen bits remaining position =
    let mostCommon = mostCommonBit remaining position               // Find most common bit of remaining values
    let result = bits |> Seq.insertAt (Seq.length bits) mostCommon  // Insert that bit at the end
    let remaining: seq<seq<int>> = remaining |> Seq.filter(fun x -> (valAt x position) = mostCommon) // Keep only the ones with the most common at the right position

    (result, remaining)
let calcNextScrubbing bits remaining position =
    let leastCommon = 
        if (Seq.length remaining) = 1 then          // If there is only 1 number left we have to take its value apparentely
            (valAt (Seq.head remaining) position) 
        else 
            leastCommonBit remaining position 
    let result = bits |> Seq.insertAt (Seq.length bits) leastCommon
    let remaining: seq<seq<int>> = remaining |> Seq.filter(fun x -> (valAt x position) = leastCommon)

    //printfn "RES: %A" result
    //printfn "REM: %A" remaining
    //printfn ""

    (result, remaining)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)
    
    // Parse ints
    let parsed = 
        input |>  
        Seq.map (fun s -> s |> Seq.map (fun c -> c |> Char.GetNumericValue |> int)) 
    let binaryLength = Seq.length (Seq.head parsed)
    printfn "Binary input length: %i" binaryLength

    let initial = Seq.replicate (Seq.head parsed |> Seq.length) 0
    let mapped = 
        parsed |>
        Seq.map (fun s -> s |> Seq.map (fun c -> if c = 0 then -1 else c)) |> // Adjust 0s to start with -1
        Seq.fold (fun acc curr -> Seq.zip acc curr |> Seq.map (fun (fst, snd) -> fst+snd)) initial |>
        Seq.map (fun x -> if x >= 0 then 1 else 0)

    let (gamma, _, _) = 
        mapped |>
        Seq.fold (fun (value, cnt, length) curr -> (value ||| (curr <<< (length - cnt - 1)), cnt + 1, length)) (0, 0, Seq.length initial)
    let epsilon = 
        bitsToInt (
            mapped |>
            Seq.map (fun x -> if x = 1 then 0 else 1)) (Seq.length initial)

    let part1 = gamma * epsilon
    
    // Specified types here to help with the debugging
    // Run through each position and use calculate the right value for that posisition
    let (oxygenRatingSeq, _) = 
        initial |> 
        Seq.indexed |>
        Seq.map (fun x -> fst x) |>
        Seq.fold (fun (bits: seq<int>, remaining: seq<seq<int>>) curr -> calcNextOxygen bits remaining curr) (Seq.empty<int>, parsed)

    let (scrubbingRatingSeq, _) =
        initial |>
        Seq.indexed |> 
        Seq.map (fun x -> fst x) |>
        Seq.fold (fun (bits, remaining) curr -> calcNextScrubbing bits remaining curr) (Seq.empty, parsed)
    //printfn "%A" oxygenRating
    
    printfn "%A" oxygenRatingSeq
    printfn "%A" scrubbingRatingSeq

    let oxygenRating = bitsToInt oxygenRatingSeq binaryLength
    let scrubbingRating = bitsToInt scrubbingRatingSeq binaryLength

    printfn "Oxygen rating: %i" oxygenRating
    printfn "Scrubbing rating: %i" scrubbingRating

    let part2 = oxygenRating * scrubbingRating

    part1.ToString(), part2.ToString()