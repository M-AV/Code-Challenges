module _2023_03

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Find all number next to a symbol in the grid and sum them up
// Task 2: Find all pairs of numbers that touches the same *. Multiply the pair and sum all of them up

let parseInput (input : string seq) = 
    input |> parsePadded2dArray '.'

let isSymbol value = 
    not (Char.IsNumber value) && value <> '.'

let getNumber (grid:char array2d) (x,y)  : int = 
    let rec parseNumber loc res =
        match grid[fst loc, snd loc] with
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> 
            let updatedRes = res * 10 + charToInt grid[fst loc, snd loc]
            parseNumber (fst loc + 1, snd loc) updatedRes
        | _ -> res
    parseNumber (x,y) 0

let isNextToSymbol loc (grid:char array2d) : bool =
    let rec traverse (x,y) isFirst =
        if isSymbol grid[x,y-1] || isSymbol grid[x,y] || isSymbol grid[x,y+1] then
            true
        else if isFirst || Char.IsNumber grid[x,y] then
            traverse (x+1, y) false
        else
            false
    traverse (fst loc - 1, snd loc) true

let rec getStartOfNumber (x,y) (grid:char array2d) : (int * int) =
    if Char.IsNumber grid[x-1, y] then
        getStartOfNumber (x-1, y) grid
    else
        (x,y)

let getGearNumber (x,y) (grid:char array2d) : int option =
    let mutable positions = [(x-1, y); (x+1, y); (x-1, y-1); (x-1, y+1)]
    if not (Char.IsNumber(grid[x, y-1])) then
        positions <- (x+1, y-1) :: positions
    else if not (Char.IsNumber(grid[x-1, y-1])) then
        positions <- (x, y-1) :: positions
        
    if not (Char.IsNumber(grid[x, y+1])) then
        positions <- (x+1, y+1) :: positions
    else if not (Char.IsNumber(grid[x-1, y+1])) then
        positions <- (x, y+1) :: positions

    let numbers = 
        positions
        |> List.map (fun (xi,yi) -> (xi,yi), Char.IsNumber grid[xi,yi])
        |> List.filter snd
        |> List.map (fun (loc, _) -> getStartOfNumber loc grid)
        |> List.map (getNumber grid)

    if numbers.Length < 2 then 
        None
    else
        Some(numbers[0] * numbers[1])


let part1 input =
    let mutable numbers = []
    for y in 1 .. Array2D.length1 input - 2 do
        for x in 1 .. Array2D.length2 input - 2 do
            if Char.IsNumber input[x,y] && not (Char.IsNumber input[x-1, y]) then
                let number = getNumber input (x,y)
                if isNextToSymbol (x,y) input then
                    numbers <- number :: numbers
                
    numbers |> List.map int64 |> List.sum

let part2 input =
    let mutable numbers = []
    for y in 1 .. Array2D.length1 input - 2 do
        for x in 1 .. Array2D.length2 input - 2 do
            if input[x,y] = '*' then
                match getGearNumber (x,y) input with
                | None -> numbers <- numbers
                | Some x -> numbers <- x :: numbers
                
    numbers |> List.sum

let execute (input : string seq) =
    let parsed = parseInput input
    printGrid2d parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "3" |> Async.RunSynchronously)
    Assert.Equal("536576", part1)
    Assert.Equal("75741499", part2)