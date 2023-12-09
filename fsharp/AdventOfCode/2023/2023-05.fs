module _2023_05

open InputProvider
open Calculations
open Parsing
open System
open Xunit

let parseInput (input : string seq) = 
    let rec groupMaps currentChunk chunks lines =
        match lines with
        | [] -> (currentChunk |> List.rev)::chunks
        | s::rest -> 
            if s = String.Empty then
                groupMaps [] ((currentChunk |> List.rev)::chunks) rest
            else
                groupMaps (s::currentChunk) chunks rest

    let rec processMap (map:string list) =
        let headerItems = (((map |> List.head).Split " ")[0]).Split "-"
        let header = headerItems[0], headerItems[2]

        let map = 
            map 
            |> List.skip 1
            |> List.map (fun x -> x.Split ' ' |> Array.map int64)
            |> List.sortBy (fun x -> x[1]) 
            |> List.map (fun x -> x[0], x[1], x[2])
        header, map

    let seeds = 
        (((input |> Seq.head).Split ": ")[1]).Split " " 
        |> Array.map int64 
        |> List.ofArray

    let rest = 
        input 
        |> Seq.skip 2 
        |> List.ofSeq 
        |> groupMaps [] []
        |> List.map processMap
        |> List.rev

    seeds, rest


// Since we ordered by src we can just iterate through them when searching
let rec mapValue value mappings =
    match mappings with
    | [] -> value // If not mapped use the same value
    | (dest, src, range)::rest ->
        if value < src then
            value
        else if value < (src + range) then
            let diff = value - src
            dest + diff
        else
            mapValue value rest
let rec searchSeed seed maps =
    match maps with 
    | [] -> seed
    | (head,map)::rest -> 
        let value = mapValue seed map
        searchSeed value rest

let part1 (seeds, (maps:((string * string) * (int64 * int64 * int64) list) list)) =
    // We can just map all values one by one and take the smallest result
    seeds
        |> List.map (fun x -> searchSeed x maps)
        |> List.min

let part2 (seeds, (maps:((string * string) * (int64 * int64 * int64) list) list)) =
    // Now there are two many values to just iterate them, so instead we track each possible 'range' of values
    // Mapping one range with a single map will result in 0, 1 or 2 new ranges, which we can then apply in the 
    // next map-section
    let pairs = 
        seeds 
        |> List.chunkBySize 2
        |> List.map (fun x -> x[0], (x[0]+x[1]-1L))

    // Take single range and single map row and return the new mapped values and which values are still unmapped values
    let matchRangeWithSingleMap ((min,max):int64*int64) ((dest,src,range):int64*int64*int64) =
        let srcMax = (src + range - 1L)

        if (max < src) || srcMax < min then // No overlap
            None, Some((min,max))
        else if src <= min && max <= srcMax then // Full overlap
            let overlapLeft = min - src
            let minMaxDist = max - min

            let newStart = dest + overlapLeft
            let newEnd = newStart + minMaxDist

            Some((newStart, newEnd)), None
        else if min <= src then // Overlap on the left
            let overlap = max - src
            
            Some(dest, dest + overlap), Some((min, src))
        else // Overlap on the right
            let overlap = srcMax - min
            let destStart = dest + (min - src)
            Some(destStart, destStart + overlap), Some((min + overlap + 1L, max))



    let rec rangeToRanges (min,max) (map:(int64*int64*int64) list) res =
    // - Map the range to all the potential ranges after matching on each map
    // - Keep track of what numbers have yet to be mapped, as those ranges have to be added in the end
        match map with
        | [] -> (min,max)::res
        | (dest, src, range)::rest -> 
            let mapRes = matchRangeWithSingleMap (min,max) (dest, src, range)

            match mapRes with
            | (None, _) -> rangeToRanges (min,max) rest res
            | (Some mapped, None) -> mapped::res
            | (Some mapped, Some remaining) -> rangeToRanges remaining rest (mapped::res)


    let rec findNewRanges map newRanges ranges =
        match ranges with
        | [] -> newRanges
        | (min,max)::rest -> 
            let foundRanges = rangeToRanges (min,max) map []

            findNewRanges map (foundRanges @ newRanges) rest

    // Fold through each map, start with inital range, should end up with a bunch of ranges.
    // Take the lowest start value of all the possible ranges
    pairs
        |> List.map (fun x -> maps |> List.fold (fun ranges currentMap -> findNewRanges (snd currentMap) [] ranges) [x])
        |> List.map (fun x -> x |> List.map fst |> List.min)
        |> List.min

let execute (input : string seq) =
    let parsed = parseInput input
    //printfn "%A" parsed

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "5" |> Async.RunSynchronously)
    Assert.Equal("157211394", part1)
    Assert.Equal("50855035", part2)