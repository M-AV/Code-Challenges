module _2022_19

open InputProvider
open Calculations
open Parsing
open System
open Xunit
open System.Text.RegularExpressions

// Task 1: Factorio time. Calc how many geodes each blueprint gives in 24 minutes 
// Task 2: Take the first three blueprints and see how many they produce in 32 minutes 

type Robot =  { 
    OreCost: int;
    ClayCost: int;
    ObsidianCost: int;
}
type Blueprint = {
    Id: int;
    Robots: Robot[];
}
let oreRegex = Regex("(\d+) ore", RegexOptions.Compiled)
let clayRegex = Regex("(\d+) clay", RegexOptions.Compiled)
let obsidianRegex = Regex("(\d+) obsidian", RegexOptions.Compiled)

let parseInput (input : string seq) = 
    input 
    |> Seq.mapi (fun i x -> i+1, x.Split('.', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun (i, x) -> i, x |> Array.map (fun r -> 
        let mutable ore = 0
        let mutable clay = 0
        let mutable obsidian = 0
        if (oreRegex.IsMatch(r)) then
            let oreMatch = (oreRegex.Matches(r))
            ore <- int (oreMatch[0].Groups[1].Value)
        if (clayRegex.IsMatch(r)) then
            let clayMatch = (clayRegex.Matches(r))
            clay <- int (clayMatch[0].Groups[1].Value)
        if (obsidianRegex.IsMatch(r)) then
            let obsiMatch = (obsidianRegex.Matches(r))
            obsidian <- int (obsiMatch[0].Groups[1].Value)
            
        { Robot.OreCost = ore; ClayCost = clay; ObsidianCost = obsidian} ))
    |> Seq.map (fun (i,x) -> { Id = i; Robots = x})
    |> List.ofSeq

// We have 5 possibilies every time. Recursively check each of the five possibilities and take the best one.
// Some optimizations to let it run in reasonably time:
// - Keep track of a global max for each blueprint. If we see that there is no way to beat that on our current path, don't continue
// - If the highest cost of, say, clay is X, don't build X+1 robots that produce clay.
// - Don't build anything in the last minute as it won't matter anyway.
// - Use Array.Parallel to calculate mmore blueprints at a time
// You could optimize it further, but this takes ~2 minutes to run per part
let part (input:Blueprint list) whichPart =
    let mutable currentMaxGlobal = Array.zeroCreate input.Length
    let rec solve blueprint time (cOre, cClay, cObsidian, cGeode) (oreRobs, clayRobs, obsiRobs, geoRobs)=
        let maxClayCost = blueprint.Robots |> Array.map (fun x -> x.ClayCost) |> Array.max
        let maxOreCost = blueprint.Robots |> Array.map (fun x -> x.OreCost) |> Array.max
        let maxObsiCost = blueprint.Robots |> Array.map (fun x -> x.ObsidianCost) |> Array.max

        if time = 1 then
            cGeode + geoRobs // With 1 minute remaining, no point in building more robots.
        else
            let next = solve blueprint (time-1)

            // Find options
            let canBuildOre =   blueprint.Robots[0].OreCost <= cOre
            let canBuildClay =  blueprint.Robots[1].OreCost <= cOre
            let canBuildObsi =  blueprint.Robots[2].OreCost <= cOre && blueprint.Robots[2].ClayCost <= cClay
            let canBuildGeode = blueprint.Robots[3].OreCost <= cOre && blueprint.Robots[3].ObsidianCost <= cObsidian

            let maxWeCanGet = (geoRobs * time) + (if canBuildGeode then (time*(time-1)) / 2 else ((time-1)*(time-2)/2))
            let mutable currentMax = currentMaxGlobal[blueprint.Id-1]
            let fourth =
                if canBuildGeode && maxWeCanGet > (currentMax - cGeode) then 
                    let inventory = cOre + oreRobs - blueprint.Robots[3].OreCost, cClay + clayRobs - blueprint.Robots[3].ClayCost, cObsidian + obsiRobs - blueprint.Robots[3].ObsidianCost, cGeode + geoRobs
                    next inventory (oreRobs, clayRobs, obsiRobs, geoRobs + 1) 
                else 0

            currentMax <- max currentMax fourth
            let third =
                // If we already have enough robots to produce anything every minute, don't create a new one
                // If there is no way to built enough obsidian robots with the time left, just cut it short here
                if canBuildObsi && obsiRobs < maxObsiCost && maxWeCanGet > (currentMax - cGeode)  then 
                    let inventory = cOre + oreRobs - blueprint.Robots[2].OreCost, cClay + clayRobs - blueprint.Robots[2].ClayCost, cObsidian + obsiRobs, cGeode + geoRobs
                    next inventory (oreRobs, clayRobs, obsiRobs + 1, geoRobs) 
                else 0

            currentMax <- max currentMax third
            let first = 
                if canBuildOre && oreRobs < maxOreCost && maxWeCanGet > (currentMax - cGeode)  then 
                    let inventory = cOre + oreRobs - blueprint.Robots[0].OreCost, cClay + clayRobs, cObsidian + obsiRobs, cGeode + geoRobs
                    next inventory (oreRobs + 1, clayRobs, obsiRobs, geoRobs) 
                else 0
            
            currentMax <- max currentMax first
            let second =
                if canBuildClay && clayRobs < maxClayCost && maxWeCanGet > (currentMax - cGeode)  then 
                    let inventory = cOre + oreRobs - blueprint.Robots[1].OreCost, cClay + clayRobs, cObsidian + obsiRobs, cGeode + geoRobs
                    next inventory (oreRobs, clayRobs + 1, obsiRobs, geoRobs) 
                else 0
            
            currentMax <- max currentMax second
            let fifth = 
                if (maxWeCanGet <= (currentMax - cGeode) ) then
                    0 // If we can't beat the current best, just stop
                else
                    // Last option is to not build anything
                    let inventory = cOre + oreRobs, cClay + clayRobs, cObsidian + obsiRobs, cGeode + geoRobs 
                    next inventory (oreRobs, clayRobs, obsiRobs, geoRobs)

            let res = [ first; second; third; fourth; fifth] |> List.max
            currentMaxGlobal[blueprint.Id-1] <- max currentMaxGlobal[blueprint.Id-1] res
            res
            
    if (whichPart = 1) then
        let res = 
            input |> Array.ofList |> Array.Parallel.map (fun b -> 
                printfn "Solving %i.." b.Id
                b.Id, solve b 24 (0,0,0,0) (1, 0, 0, 0)) |> Seq.map (fun (x,y) -> x * y) |> Seq.sum

        res
    else
        let res = 
            input |> Seq.take 3 |> Array.ofSeq |> Array.Parallel.map (fun b -> 
                printfn "Solving %i.." b.Id
                solve b 32 (0,0,0,0) (1, 0, 0, 0)) |> Array.ofSeq

        //printfn "%A" res
        res[0]*res[1]*res[2]
let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let part1 = part parsed 1

    let part2 = part parsed 2

    part1.ToString(), part2.ToString()

//[<Fact>] // Each part takes about 2 minutes to execute
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "19" |> Async.RunSynchronously)
    Assert.Equal("1565", part1)
    Assert.Equal("10672", part2)