module _2022_7

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// NONE OF THIS WORKS!

// Task 1: Parse folder structure and find every folder with size at 100000 or less and sum it.
// Task 2:

type File = int * string

type Node =
    | File of int * string
    | Dir of string * Map<string, Node> * Option<Node>

//let getDir (node:Node) =
//    match node with
//    | Dir(n,c) -> Dir(n,c)

let parseInput (input : string seq): Node = 
    input
    //let parseFile line =
    //    let split = file.Split ' '
    //    File(split[0] |> int, split[1])

    //let rec parse2 remanining (current:Node) : Node=
    //    match remanining with
    //    | [] -> current
    //    | head::tail ->
    //        match head with 
    //        | "" -> current
    //        | Prefix "$ cd " rest ->
    //            match current with
    //            | Dir(name, children, parent) ->
    //                let d = Dir(name, children |> Map.add rest parsedDir, parent)
    //                parent.Value <- d
    //                parse tail parsed d
    //            let ch = current
    //            //let folder = parse2 tail 
    //            current
    //        | Prefix "$ ls" rest -> 
    //            let toProcess = tail |> List.takeWhile(fun x -> !x.StartsWith('$'))
    //            let left = tail |> List.skip toProcess.Length

    //            let processed = 
    //                toProcess 
    //                |> List.filter (fun x -> !(x.StartsWith "dir ")) 
    //                |> List.iter parseFile
    //                |> List.map (fun (size, name) -> (name, File(size, name)))


                
    //        | Prefix "dir " rest -> 
    //            Dir(rest, Map.empty, Some(current))
    //        | file ->
    //            let split = file.Split ' '
    //            File(split[0] |> int, split[1])



    //let rec parse remaining parsed (current:Node)=
    //    match remaining with 
    //    | [] -> parsed
    //    | head::tail -> 
    //        match head with
    //        | Prefix "$ cd /" rest -> 
    //            let treeNode = Dir("/", Map.empty, None)
    //            parse tail treeNode treeNode
    //        | Prefix "$ cd .." rest ->
    //            match current with
    //            | Dir(_, _, parent) -> parse tail parsed parent.Value
    //        | Prefix "$ cd " rest -> 
    //            // Current has to be a directory, so we don't bother checking for File
    //            match current with
    //            | Dir(name, children, parent) -> parse tail parsed children[rest]
    //        | Prefix "$ ls" rest ->
    //            parse tail parsed current
    //        | Prefix "dir " rest ->
    //            let parsedDir = Dir(rest, Map.empty, Some(current))
    //            match current with
    //            | Dir(name, children, parent) ->
    //                let d = Dir(name, children |> Map.add rest parsedDir, parent)
    //                parent.Value <- d
    //                parse tail parsed d
    //        | file -> 
    //            let split = file.Split ' '
    //            let parsedFile = File(split[0] |> int, split[1])
    //            match current with
    //            | Dir(name, children, parent) -> 
    //                let s = children |> Map.add split[1] parsedFile
    //                let newCurrent = Dir(name, s, parent)
    //                parse tail parsed newCurrent
    //let root = Dir("/", Map.empty, None);

    //parse (input |> List.ofSeq) root root

let rec printDirectory (input:Node) depth =
    match input with
    | File(size, name) -> 
        printfn "%s" (new String('-', depth) + " " + name + " (" + size.ToString() + ")")
    | Dir(name, children, parent) ->
        printfn "%s" (new String('-', depth) + " " + name)
        children |> Map.iter (fun x y -> printDirectory y (depth + 1))

//let part1 input = 
//    let rec solve remaining sum =
//        match remaining with 
//        | [] -> sum
//        | head::tail -> 
//            match head with 
//            | '

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printDirectory parsed 1
    //printfn "%A" parsed

    let part1 = "N/A"

    let part2 = "N/A"

    part1.ToString(), part2.ToString()

//[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "7" |> Async.RunSynchronously)
    Assert.Equal("N/A", part1)
    Assert.Equal("N/A", part2)