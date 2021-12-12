module _2021_12

open Xunit
open System
open System.Linq
open System.Collections.Generic

// Task 1: Find number of unique paths from start to end (we can visit big letters more than once)
// Task 2: 

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
        let fst = if first.Any(fun x -> Char.IsUpper x) then Large(first) else Small(first)
        let snd = if second.Any(fun x -> Char.IsUpper x) then Large(second) else Small(second)
        [ Edge(fst, snd); Edge(snd, fst) ])
    |> Seq.collect id
    |> Seq.groupBy (fun (src, _) -> src)
    |> Seq.map (fun (src, dest) -> (src, dest |> Seq.map (fun (_, dest) -> dest) |> List.ofSeq))
    |> Map.ofSeq

let findAllPaths (input:Map<Cave, Cave list>) =
    let rec bfs (current:Cave) (visited:Set<Cave>) =
        if visited.Contains current then 0
        else
            match current with
            | Small(name) when name = "end" -> 1
            | Small(name) -> 
                match input[current] with
                | [] -> 0
                | paths -> 
                    paths 
                    |> List.map (fun cave -> bfs cave (visited.Add current)) |> List.sum
            | Large(name) -> 
                match input[current] with
                | [] -> 0
                | paths ->
                    paths |> List.map (fun cave -> bfs cave visited) |> List.sum

    
    bfs (Small("start")) Set.empty

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let x = findAllPaths parsed
    printfn "%A" x

    let part1 = x

    let part2 = "N/A"

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
    let (part1, _) = execute input
    Assert.Equal("19", part1)

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
    let (part1, _) = execute input
    Assert.Equal("226", part1)