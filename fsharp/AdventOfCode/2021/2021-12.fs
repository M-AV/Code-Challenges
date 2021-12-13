module _2021_12

open Xunit
open System

// Task 1: Find number of unique paths from start to end (we can visit big letters more than once)
// Task 2: Find number of paths if we can visit a single small cave twice 

type Cave =
    | Small of string
    | Large of string

type Edge = Cave * Cave

let parseInput (input : string seq) =
    input 
    |> Seq.map (fun x -> x.Split '-')
    |> Seq.map (fun x -> (x[0], x[1]))
    |> Seq.map (
        fun (first, second) -> 
        let fst = if first.[0] |> Char.IsUpper then Large(first) else Small(first)
        let snd = if second.[0] |> Char.IsUpper then Large(second) else Small(second)
        [ Edge(fst, snd); Edge(snd, fst) ])
    |> Seq.collect id
    |> Seq.groupBy (fun (src, _) -> src)
    |> Seq.map (fun (src, dest) -> (src, dest |> Seq.map (fun (_, dest) -> dest) |> List.ofSeq))
    |> Map.ofSeq

// The optional Cave indicates if we have to visit that one twice in the solution
let findAllPaths (input:Map<Cave, Cave list>) (visitTwice:Cave option) =
    let rec dfs (current:Cave) (visited:Set<Cave>) (bonusVisit:Cave option) (path:Cave list) = // We're not using the path, but it was helpful for debugging purposes
        if visited.Contains current then 0
        else
            match current, bonusVisit with
            | Small(name), None when name = "end" -> 
                match visitTwice with 
                | Some(x) when visited.Contains(x) -> 1 // Make sure we have visited it twice if we have to
                | Some(x) -> 0
                | _ -> 1
            | Small(name), Some cave when name = "end" -> 0
            | Small(name), Some cave when cave = current ->
                match input[current] with
                | [] -> 0
                | paths ->
                    paths 
                    |> List.map (fun cave -> dfs cave visited None (current :: path)) |> List.sum
            | Small(name), x -> 
                match input[current] with
                | [] -> 0
                | paths -> 
                    paths 
                    |> List.map (fun cave -> dfs cave (visited.Add current) x (current :: path)) |> List.sum
            | Large(name), x -> 
                match input[current] with
                | [] -> 0
                | paths ->
                    paths |> List.map (fun cave -> dfs cave visited x (current :: path)) |> List.sum

    
    dfs (Small("start")) Set.empty visitTwice []

// Here we just bruteforce every single node. It should be possible to make a solution that does only 1 search
let findAllPaths_Part2 (input:Map<Cave, Cave list>) =
    let isSmall cave =
        match cave with
        | Small(_) -> true
        | Large(_) -> false
    let smallCaves = 
        input.Keys 
        |> Seq.filter isSmall
        |> Seq.filter (fun x -> x <> Small("start") && x <> Small("end"))

    let numberOfDoubleVisitPaths = 
        smallCaves 
        |> Seq.map (fun x -> findAllPaths input (Some(x)))
        |> Seq.sum
    numberOfDoubleVisitPaths + (findAllPaths input None)

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = findAllPaths parsed None

    let part2 = findAllPaths_Part2 parsed

    part1.ToString(), part2.ToString()


[<Fact>]
let ``Example 1``() =
    let input = 
        [ "start-A";
          "start-b";
          "A-c";
          "A-b";
          "b-d";
          "A-end";
          "b-end" ]
    let (part1, part2) = execute input
    Assert.Equal("10", part1)
    Assert.Equal("36", part2)
[<Fact>]
let ``Example 2``() =
    let input = 
        [ "dc-end";
          "HN-start";
          "start-kj";
          "dc-start";
          "dc-HN";
          "LN-dc";
          "HN-end";
          "kj-sa";
          "kj-HN";
          "kj-dc" ]
    let (part1, part2) = execute input
    Assert.Equal("19", part1)
    Assert.Equal("103", part2)

[<Fact>]
let ``Example 3``() =
    let input = 
        [ "fs-end";
        "he-DX";
        "fs-he";
        "start-DX";
        "pj-DX";
        "end-zg";
        "zg-sl";
        "zg-pj";
        "pj-he";
        "RW-he";
        "fs-DX";
        "pj-RW";
        "zg-RW";
        "start-pj";
        "he-WI";
        "zg-he";
        "pj-fs";
        "start-RW" ]
    let (part1, part2) = execute input
    Assert.Equal("226", part1)
    Assert.Equal("3509", part2)