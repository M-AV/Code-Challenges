module _2022_16

open InputProvider
open Calculations
open Parsing
open System
open System.Collections.Generic
open Xunit

// Task 1: Given the valves and the graph of tunnels, figure out how much pressure you can let out at most in 30 minutes.
// Task 2: Give 1 elephant some of the valves, then figure out how much pressure you can let out total in 26 minutes

let parseInput (input : string seq) = 
    let parsed = 
        input
        |> Seq.map (fun x -> x.Substring 6)
        |> Seq.map (fun x -> x[0..1], x.Substring 17)
        |> Seq.map (fun (n, x) -> 
            let idx = x.IndexOf ';'
            let s = if (x.IndexOf "valves") = -1 then 24 else 25
            (n, int x[0..idx-1]), x[idx+s..].Split ", " |> Array.map (fun d -> (d, 1)))
        |> List.ofSeq

    // For faster lookup we translate the names to integers
    let newNames = 
        parsed
        |> Seq.sortBy (fun x -> fst (fst x))
        |> Seq.indexed
        |> Seq.map (fun x -> fst (fst (snd x)), fst x)
        |> Map.ofSeq

    let mutable destMap = 
        parsed 
        |> Seq.map (fun ((n,_), dest) -> (n, dest)) 
        |> Seq.map (fun (name, dests) -> newNames[name], dests |> Array.map (fun (x,i) -> (newNames[x],i)))
        |> Map.ofSeq

    let flowMap = 
        parsed
        |> Seq.map fst
        |> Seq.map (fun (x,y) -> (newNames[x],y))
        |> Map.ofSeq

    let workingValves = parsed |> Seq.map fst |> Seq.map snd |> Seq.filter (fun x -> x > 0) |> Seq.length

    // Reduce - We remove all valves that only connects to 2 and has flow 0. We could theoretically remove all valves with flow 0
    for k in destMap do
        let key = k.Key
        let value = k.Value
        if (value.Length = 2 && flowMap[key] = 0 && key <> 0) then
            let srcDests = destMap[fst value[0]]
            printfn "BEF: %A  %A" (fst value[0]) srcDests
            let idx = srcDests |> Array.findIndex (fun (n,_) -> n = key)

            srcDests[idx] <- (fst value[1]), (snd srcDests[idx]) + (snd value[1])
            printfn "AFT: %A   %A" (fst value[0]) srcDests

            let trgDests = destMap[fst value[1]]
            printfn "BEF: %A   %A" (fst value[1]) trgDests
            let idx2 = trgDests |> Array.findIndex (fun (n, _) -> n = key)
            trgDests[idx2] <- (fst value[0]), (snd trgDests[idx2]) + (snd value[0])
            printfn "AFT: %A   %A" (fst value[1]) trgDests

            destMap <- destMap.Remove key
            //printfn "%A" idx

    destMap, flowMap, workingValves
    
// Slow solution. Runs example in 0.1 second, but takes a bit more than 6 minutes to run on the problem (in Release) 
// Idea here was to recursively decide which valve to go to from the current valve and check all cases.
// I did a few tricks to optimize, such as stopping if we hit the same node more times than it has exits, but it isn't enough
// for it to perform. 
// I did use it to calculate the result for part 1, so I'm leaving it here.
let part1_slow ((destMap:Map<int, (int * int) array>), (flowMap:Map<int, int>), workingValves) =
    let stateMap = destMap |> Map.keys |> Seq.map (fun x -> x, false) |> Map.ofSeq
    printfn "%A" stateMap
    let startValve = 0

    // My argument list is getting ridiculous....
    let rec solve prevValve currentValve valvesOpen remTime (flow:int) (states:Map<int, bool>) (path:int list) (visitCnt:Map<int, int>): int=
        let updatedVisitCnt = visitCnt.Change(currentValve, fun x -> match x with | Some s -> Some(s + 1) | None -> Some(1))
        if (remTime <= 1 || valvesOpen = workingValves) then // If out of time or we opened all valves with a flow higher than 0
            flow
        elif (updatedVisitCnt[currentValve] > destMap[currentValve].Length) then // If we have visited this node more times than we have exits, we're in a loop and might as well exit this path (Cut ~8/9 of the time off the example case)
            flow
        else
            let mutable ifOpening = 0
            // If opening
            if (not states[currentValve] && flowMap[currentValve] > 0) then
                let addedFlow = flowMap[currentValve] * (remTime - 1)
                let newFlow = flow + addedFlow

                ifOpening <- 
                    destMap[currentValve]
                    |> Seq.map (fun (n, d) -> 
                        solve currentValve n (valvesOpen + 1) (remTime - (d+1)) newFlow (states.Add(currentValve, true)) (currentValve::path) updatedVisitCnt)
                    |> Seq.max
            
            let dests = destMap[currentValve]
            if ((Array.length dests) = 1 && (fst dests[0]) = prevValve) then 
                ifOpening
            else
                let ifNotOpening = 
                    destMap[currentValve] 
                    |> Seq.filter (fun (x,d) -> x <> prevValve) // You would never go to a valve and back immediately if not opening the current valve
                    |> Seq.map (fun (x,d) -> solve currentValve x valvesOpen (remTime - d) flow states (currentValve::path) updatedVisitCnt) 
                    //|> Seq.map (fun (x,d) -> solve currentValve x valvesOpen (remTime - d) flow states) 
                    |> Seq.max
                max ifOpening ifNotOpening



    solve startValve startValve 0 30 0 stateMap [] Map.empty

let findDistances ((destMap:Map<int, (int * int) array>), (flowMap:Map<int, int>), workingValves) =
    let x = destMap.Keys |> Seq.max

    let mutable distances = Array2D.init (x+1) (x+1) (fun x y -> Int32.MaxValue)

    // Dijkstra - run for each node
    for currentSource in (destMap.Keys) do
        let queue = PriorityQueue<int, int>()
        distances[currentSource, currentSource] <- 0

        queue.Enqueue(currentSource, 0)

        while queue.Count > 0 do
            let node = queue.Dequeue()

            for (name, edgeLength) in destMap[node] do
                let dist = distances[currentSource, node] + edgeLength
                if (dist < distances[currentSource, name]) then
                    distances[currentSource, name] <- dist
                    queue.Enqueue(name, dist)

    
    distances

let reusableSolve (flowMap:Map<int, int>) (distances:int array2d) (workingValves:Set<int>) (timeLeft:int): int =
    let rec solve currentValve currentFlow remTime remaining =
        let options = remaining |> Set.filter (fun x -> distances[currentValve, x] < (remTime-1)) // Find all valves we can reach with time left - Account for time to open valve
        let newFlow = flowMap[currentValve] * (remTime - 1) + currentFlow

        if (Set.isEmpty options) then
            newFlow
        else
            let sss =
                options
                |> Seq.map (fun x -> 
                    let updatedRemTime = remTime - 1 - distances[currentValve, x] // Use 1 minute to open valve and then travel
                    solve x newFlow updatedRemTime (remaining |> Set.remove x))
                |> Seq.max

            sss

    let result = 
        workingValves 
        |> Seq.map (fun x -> 
            let distToStart = distances[0, x]
            solve x 0 (timeLeft-distToStart) (workingValves |> Set.remove x))
        |> Seq.max

    result

// Here we filter out all valves with 0 flow and use a map of all distances between all valves instead.
// Then we pick a valve and say "If this is the first valve I open, what is the best of the remaining valves".
// Doing this we basically check all combinations until we find the right one.
// This runs in ~0.35 seconds on my machine in Debug mode
let part1 ((destMap:Map<int, (int * int) array>), (flowMap:Map<int, int>), workingValvesCount) (distances:int array2d) : int =
    let workingValves = flowMap |> Seq.filter (fun x -> x.Value > 0) |> Seq.map (fun x -> x.Key) |> Set.ofSeq

    reusableSolve flowMap distances workingValves 30

// We simply split the valves into 2 in all possible permutations and see how much pressure we can release if 
// the valves were split like that. Then take the max of all of those. Runs in ~24.5 seconds in Debug mode
let part2 ((destMap:Map<int, (int * int) array>), (flowMap:Map<int, int>), workingValvesCount) (distances:int array2d) = 

    let workingValves = flowMap |> Seq.filter (fun x -> x.Value > 0) |> Seq.map (fun x -> x.Key) |> Set.ofSeq

    let powerset = powerset (workingValves |> List.ofSeq)

    let res = 
        powerset 
        |> Seq.filter (fun x -> x.Length <= (workingValves.Count / 2)) // No point in looking at mirrored sets
        |> Seq.filter (fun x -> x.Length > 0) // No point in looking at empty sets either as that is just part 1 with less minutes
        |> Seq.map (fun x -> 
            // Find remaining valves for the elf and calculate
            let elfValves = x |> List.fold (fun agg cur -> agg |> Set.remove cur) workingValves
            let elephantValves = x |> Set.ofList
            let elefRes = reusableSolve flowMap distances elephantValves 26
            let elfRes = reusableSolve flowMap distances elfValves 26
            
            elefRes + elfRes
        )
        |> Seq.max

    res

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let distances = findDistances parsed

    let part1 = part1 parsed distances

    let part2 = part2 parsed distances

    part1.ToString(), part2.ToString()

//[<Fact>] // Takes ~25 seconds
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "16" |> Async.RunSynchronously)
    Assert.Equal("2059", part1)
    Assert.Equal("2790", part2)