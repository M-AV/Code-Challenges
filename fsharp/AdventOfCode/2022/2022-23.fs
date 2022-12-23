module _2022_23

open InputProvider
open Calculations
open Parsing
open System
open Xunit

// Task 1: Move elfs according to rules on website. Assume they are in a grid, find number of empty squares
// Task 2: Find out how many iterations of moves until no elf is moving

// Parse into a map of positions
let parseInput (input : string seq) = 
    let arr = 
        input 
        |> parseArray2d

    let mutable positions = Set.empty
    arr |> Array2D.iteri (fun x y v -> 
        if v = '#' then
            positions <- positions |> Set.add (x,y))

    positions

let directions = [| 
    fun (x,y) -> [| x-1,y-1; x, y-1; x+1, y-1 |]; // N
    fun (x,y) -> [| x-1,y+1; x, y+1; x+1, y+1 |]; // S
    fun (x,y) -> [| x-1,y-1; x-1, y; x-1, y+1 |]; // W
    fun (x,y) -> [| x+1,y-1; x+1, y; x+1, y+1 |]; // E
|]
    
// For each elf, iterate through the directions and see if we can find a proposed move.
// If not, just return None.
// In the end we group everyone into a map of *proposed position* -> *elfs with that proposal*.
// This should give us a bunch of elfs without proposals (None -> elfs) and a bunch of proposals.
let findProposals positions globalDirIdx =
    let rec proposal (x,y) dirIdx : (int*int) option=
        if dirIdx = 4 then
            None
        else
            // There will be dupes, but let's see if it matters
            let hasNeighbors = 
                directions 
                |> Array.map (fun f -> f(x,y))
                |> Array.collect (fun f -> f)
                |> Array.exists (fun pos -> positions |> Set.contains pos)

            if not hasNeighbors then
                None
            else
                let adjacents = directions[(dirIdx + globalDirIdx) % 4](x,y)
                if adjacents |> Array.exists (fun adj -> positions |> Set.contains adj) then
                    proposal (x,y) (dirIdx+1)
                else
                    Some(adjacents[1])
                

    let proposals = 
        positions 
        |> Seq.map (fun pos -> proposal pos 0, pos)
        |> List.ofSeq
        |> List.groupBy fst
        |> List.map (fun (x,y) -> x, y |> List.map snd)
        
    proposals
 
// Move all elfs that has a unique proposal
let rec applyProposals positions proposals =
    match proposals with
    | [] -> positions
    | (newLoc, elfs)::tail ->
        match newLoc with
        | None -> applyProposals positions tail
        | Some loc -> 
            if (elfs |> List.length) > 1 then
                applyProposals positions tail
            else
                let updatedPos = positions |> Set.remove (elfs.Head) |> Set.add loc
                applyProposals updatedPos tail

// Run 10 times and then calculate size of grid
let part1 (input:Set<int*int>) =
    let finalPos, _ = 
        [0..9]
        |> List.fold (fun (pos, cnt) i -> 
            if cnt = 1 then
                pos, 1
            else
                let props = findProposals pos (i % 4)
                let newPos = applyProposals pos props
                newPos, props.Length
            ) (input, 0)

    
    let smallestX, _ = finalPos |> Seq.sortBy fst |> Seq.head
    let _, smallestY = finalPos |> Seq.sortBy snd |> Seq.head
    let largestX,_ = finalPos |> Seq.sortByDescending fst |> Seq.head
    let _,largestY = finalPos |> Seq.sortByDescending snd |> Seq.head

    let diffToZeroX = 0 - smallestX
    let diffToZeroY = 0 - smallestY
    let gridSize = (largestX+1 + diffToZeroX) * (largestY+1 + diffToZeroY)

    gridSize - (input.Count)

// Run until all elfs are in the "None -> elfs" group and count how many iterations
let part2 input =
    let mutable iteration = 0
    let mutable positions = input
    let mutable changeCount = 0

    while changeCount <> 1 do
        let props = findProposals positions (iteration % 4)
        positions <- applyProposals positions props
        changeCount <- props.Length
        iteration <- iteration + 1

    iteration

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    //printfn "%part1rsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

//[<Fact>] // Part 2 takes ~30 seconds
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "23" |> Async.RunSynchronously)
    Assert.Equal("4114", part1)
    Assert.Equal("970", part2)