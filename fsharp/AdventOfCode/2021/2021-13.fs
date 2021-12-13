module _2021_13

open Xunit

// Task 1: Fold paper once at the designated line and count how many dots
// Task 2: Fold remaining times and read code

type Point = int * int
type FoldIns = char * int

let parseInput (input : string seq) =
    let dots = 
        input 
        |> Seq.filter (fun x -> x.Contains ',')
        |> Seq.map (fun x -> x.Split ',')
        |> Seq.map (fun x -> Point(int x[0], int x[1]))
        |> List.ofSeq
    let instructions =
        input 
        |> Seq.filter (fun x -> x.Contains "fold")
        |> Seq.map (fun x -> x.Split '=')
        |> Seq.map (fun x -> FoldIns(x[0][x[0].Length - 1], int x[1]))
        |> List.ofSeq

    (dots, instructions)

let hasOverlapping foldIns dot dots =
    if fst foldIns = 'y' then
        dots 
        |> Seq.filter (fun (x,y) -> x = fst dot)
        |> Seq.filter (fun (x,y) -> y = (snd foldIns * 2) - (snd dot))
        |> Seq.length > 0
    else
        dots 
        |> Seq.filter (fun (x,y) -> y = snd dot)
        |> Seq.filter (fun (x,y) -> x = (snd foldIns * 2) - (fst dot))
        |> Seq.length > 0

let foldPoint foldIns dot =
    if fst foldIns = 'y' then
        if snd dot <= snd foldIns then 
            dot
        else
            (fst dot, (snd dot - (snd dot - snd foldIns) * 2))
    else
        if fst dot <= snd foldIns then 
            dot
        else
            (fst dot - (fst dot - snd foldIns) * 2, snd dot)

let rec foldPaper instructions dots =
    match instructions with
    | [] -> dots
    | head :: tail -> foldPaper tail (dots |> List.map (fun x -> foldPoint head x)) |> List.distinct

let printPaper dots =
    let map = dots |> Set.ofList
    for y in 0 .. (dots |> List.map snd |> List.max) do
        for x in 0 .. (dots |> List.map fst |> List.max) do
            if map.Contains (x,y) then
                printf "# "
            else
                printf ". "
        printfn ""

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let (dots, instr) = parseInput input

    let part1 = foldPaper ([ instr |> List.head ]) dots |> Seq.length

    let dotsAfterFold = foldPaper instr dots

    printPaper dotsAfterFold

    let part2 = "See print"

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Example 1``() =
    let input = 
        [ 
        "6,10";
        "0,14";
        "9,10";
        "0,3";
        "10,4";
        "4,11";
        "6,0";
        "6,12";
        "4,1";
        "0,13";
        "10,12";
        "3,4";
        "3,0";
        "8,4";
        "1,10";
        "2,14";
        "8,10";
        "9,0";
        "";
        "fold along y=7";
        "fold along x=5"; ]
    let (part1, part2) = execute input
    Assert.Equal("17", part1)
    //Assert.Equal("", part2)