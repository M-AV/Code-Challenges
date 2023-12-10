module _2023_08

open InputProvider
open Calculations
open Parsing
open System
open Xunit

let parseInput (input : string seq) = 
    let dirs = input |> Seq.head |> List.ofSeq

    let map = 
        input 
        |> Seq.skip 2
        |> Seq.map (fun x -> x.Split " = ")
        |> Seq.map (fun x -> x[0], x[1].Split ", ")
        |> Seq.map (fun (x,y) -> x, (y[0].Replace("(", ""), y[1].Replace(")", "")))
        |> Map.ofSeq

    dirs, map

let part1 (dirs, map) =
    let rec traverse currentPos path count =
        let (left, right) = Map.find currentPos map
        match currentPos, path with
        | _, [] -> 
            traverse currentPos dirs count
        | "ZZZ", _ -> count
        | _, 'L'::rest -> 
            traverse left rest (count + 1)
        | _, 'R'::rest -> 
            traverse right rest (count + 1)

    traverse "AAA" dirs 0

let part2 (dirs, map) =
    
    let startingNodes = map |> Map.keys |> List.ofSeq |> Seq.filter (fun (x:string) -> x[2] = 'A') |> List.ofSeq

    let isDone nodes =
        let cnt = nodes |> List.filter (fun (x:string) -> x[2] = 'Z') |> List.length
        cnt = (List.length nodes)

    let findNextNode dir node =
        match dir with
        | 'L' -> fst (Map.find node map)
        | 'R' -> snd (Map.find node map)

    let rec traverse currentPos path (count:int64) =
        if isDone currentPos then
            count
        else
            match path with
            | [] -> traverse currentPos dirs count
            | dir::rest -> 
                let newPos = currentPos |> List.map (findNextNode dir)
                traverse newPos rest (count+1L)

    // Initially I just tried to simluate all the required moves.. It took a long time.
    //traverse startingNodes dirs 0L

    let rec loopLength (pos:string) path idx = 
        if pos[2] = 'Z' then
            idx
        else
            match path with
            | [] -> loopLength pos dirs idx
            | dir::rest -> loopLength (findNextNode dir pos) rest (idx+1)
           
    
    // Urgh.. The naive solution of just simulating the moves takes too long. After being unable to 
    // find a solution, I looked online for inspiration. Turns out that you have the following guarantees:
    // - Each starting point only reaches 1 exit point
    // - Each starting point loops and each loop has the exact same length from the starting point
    // 
    // These guarantees are not specified in the problem, but can be deduced from the input.
    // This makes it possible to just calculate the length of each loop and find the least common multiple.

    // Not a fan of this type of problem, but oh well.

    let loopLengths = startingNodes |> List.map (fun x -> loopLength x dirs 0) |> List.map int64
    lcm loopLengths


let execute (input : string seq) =
    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "8" |> Async.RunSynchronously)
    Assert.Equal("16043", part1)
    Assert.Equal("15726453850399", part2)