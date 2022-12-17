module _2022_16

open InputProvider
open Calculations
open Parsing
open System
open System.Collections.Generic
open Xunit

// Task 1: 
// Task 2: 

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

    let mutable destMap = 
        parsed 
        |> Seq.map (fun ((n,_), dest) -> (n, dest)) 
        |> Map.ofSeq
    let flowMap = 
        parsed
        |> Seq.map fst
        |> Map.ofSeq

    let workingValves = parsed |> Seq.map fst |> Seq.map snd |> Seq.filter (fun x -> x > 0) |> Seq.length

    // Reduce
    for k in destMap do
        let key = k.Key
        let value = k.Value
        if (value.Length = 2 && flowMap[key] = 0 && key <> "AA") then
            

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
    
// Bad and slow solution. Runs example in 0.1 second, but takes a bit more than 7 minutes to run on the problem (in Release) 
let part1_slow ((destMap:Map<string, (string * int) array>), (flowMap:Map<string, int>), workingValves) =
    let stateMap = destMap |> Map.keys |> Seq.map (fun x -> x, false) |> Map.ofSeq
    printfn "%A" stateMap
    let startValve = "AA"

    // My argument list is getting ridiculous....
    let rec solve prevValve currentValve valvesOpen remTime (flow:int) (states:Map<string, bool>) (path:string list) (visitCnt:Map<string, int>): int=
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

                //ifOpening <- solve currentValve (remTime-1) newFlow (states.Add(currentValve, true))
            
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

let findDistances ((destMap:Map<string, (string * int) array>), (flowMap:Map<string, int>), workingValves) =
    let mutable distances = (destMap.Keys |> Seq.allPairs destMap.Keys) |> Seq.map (fun pair -> pair, Int32.MaxValue) |> Map.ofSeq

    // Dijkstra - run for each node
    for currentSource in (destMap.Keys) do
        let queue = PriorityQueue<string, int>()
        distances <- (distances |> Map.add (currentSource,currentSource) 0)

        queue.Enqueue(currentSource, 0)

        while queue.Count > 0 do
            let node = queue.Dequeue()

            for (name, edgeLength) in destMap[node] do
                let prevDistKey = (currentSource, node)
                let distKey = (currentSource, name)
                let dist = distances[prevDistKey] + edgeLength
                if (dist < distances[distKey]) then
                    distances <- distances.Add(distKey, dist)
                    queue.Enqueue(name, dist)

    
    distances

let part1 ((destMap:Map<string, (string * int) array>), (flowMap:Map<string, int>), workingValves) (distances:Map<string*string, int>) : int =
    let workingValves = flowMap |> Seq.filter (fun x -> x.Value > 0) |> Seq.map (fun x -> x.Key) |> Set.ofSeq

    let rec solve currentValve currentFlow remTime remaining =
        
        let options = remaining |> Set.filter (fun x -> distances[(currentValve, x)] < remTime)

        let newFlow = flowMap[currentValve] * (remTime - 1) + currentFlow

        if (Set.isEmpty options) then
            newFlow
        else
            let sss =
                options
                |> Seq.map (fun x -> 
                    let updatedRemTime = remTime - 1 - distances[(currentValve, x)]
                    let updatedFlow = flowMap[currentValve] * (remTime - 1) + currentFlow
                    solve x updatedFlow updatedRemTime (remaining |> Set.remove currentValve))
                |> Seq.max

            sss

    let result = 
        workingValves 
        |> Seq.map (fun x -> 
            let distToStart = distances[("AA", x)]
            solve x 0 (30-distToStart) workingValves)
        |> Seq.max

    result

let part2 input = 
    2

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    printfn "%A" parsed

    let distances = findDistances parsed
    //printfn "DISTANCES"
    //distances |> Seq.iter (fun x -> printfn "%A" x)
    //printfn "%A" distances

    let part1 = part1 parsed distances

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

//[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2022" "16" |> Async.RunSynchronously)
    Assert.Equal("2059", part1)
    Assert.Equal("N/A", part2)