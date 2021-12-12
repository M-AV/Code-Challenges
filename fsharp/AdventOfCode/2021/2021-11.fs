module _2021_11

// Task 1: Simulate flashes and count how many after 100 iterations
// Task 2: Find first iteration where all octopuses flash at the same time

type location = int * int

let parseInput (input:string seq) =
    input 
    |> Seq.map (fun x -> x |> Seq.map (fun y -> (int y) - (int '0')))
    |> Seq.map Array.ofSeq
    |> Array.ofSeq

let neighborMap = [ 
    (-1,-1); (0,-1); (1,-1);
    (-1, 0);         (1, 0);
    (-1, 1); (0, 1); (1, 1)
]
let getNeighbours (loc:location) =
    neighborMap 
    |> List.map (fun (x,y) -> ((fst loc) + x, (snd loc) + y))
    |> List.filter (fun (x,y) -> x >= 0 && y >= 0)
    |> List.filter (fun (x,y) -> x < 10 && y < 10)

// Thought I would try with a mostly iterative with multidimensional array approach
// .. Might be easier with a simple (x,y) to value map.. but let's see what we can do
let simulateFlashes count (input:int[][]) =
    let addOne() =
        for i in 0 .. input.Length - 1 do
            for j in 0 .. input[i].Length - 1 do
                input[i][j] <- input[i][j] + 1

    let findInitialFlashes() =
        input
        |> Seq.indexed
        |> Seq.map (fun (idx, row) -> 
            row 
            |> Seq.indexed 
            |> Seq.filter (fun (_, value) -> value = 10)
            |> Seq.map (fun (innerIdx, _) -> (idx, innerIdx)))
        |> Seq.collect id
        |> List.ofSeq

    let rec flash (input:int[][]) (locations:location list) =
        match locations with
        | head :: tail -> 
            match input[fst head][snd head] with
            | 10 -> 
                input[fst head][snd head] <- 11
                flash input (getNeighbours head @ tail)
            | 9 ->
                input[fst head][snd head] <- 10
                flash input (head :: tail)
            | x -> // if below 10 its not ready, if above we already processed it
                input[fst head][snd head] <- x + 1
                flash input tail
        | [] -> ()

    let flashCounts = 
        [| for i in 0 .. input.Length - 1 ->
            [| for _ in 1 .. input[i].Length -> 0 |] 
        |]
    
    let resetIfFlashed() =
        for i in 0 .. input.Length - 1 do
            for j in 0 .. input[i].Length - 1 do
                if input[i][j] > 9 then
                    flashCounts[i][j] <- flashCounts[i][j] + 1
                    input[i][j] <- 0

    for _ in 1 .. count do
        addOne()
        let initialFlashes = findInitialFlashes()
        flash input initialFlashes
        resetIfFlashed()


    //printfn "%A" input
    flashCounts |> Seq.collect id |> Seq.sum

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    let part1 = simulateFlashes 100 parsed


    let newParsed = parseInput input // We're mutating the input above. So the easiest way to 'reset' is just to parse it again
    let mutable flashCount = 0
    let mutable iterations = 0
    while flashCount <> 100 do
        flashCount <- simulateFlashes 1 newParsed
        iterations <- iterations + 1



    let part2 = iterations

    part1.ToString(), part2.ToString()