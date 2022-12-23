module _2021_19

// Task 1: Match regions and count distinct beacons
// Task 2: Find max Manhattan distance between two scanners

// This solution is quite slow. Takes a few minutes to run on my machine. There are various optimizations that 
// could be made - some of them described in comments.

type Beacon = int*int*int

let parseInput (input: string seq) = 
    let parseScannerBeacons (readings: string seq) =
        readings
        |> Seq.map (fun x -> 
            x.Split(',') 
            |> Array.map int)
        |> Seq.map (fun x -> Beacon(x[0],x[1],x[2]))
        |> Set.ofSeq

    let i = ref 0
    let groups = 
        input
        |> Seq.filter (fun x -> x <> "")
        |> Seq.groupBy (fun x -> 
            if x.StartsWith("---") then 
                i.Value <- i.Value + 1
            i.Value)
        |> Seq.map (fun (i, x) -> (i, x |> Seq.filter (fun i -> not (i.StartsWith("--")))))
        |> Seq.map snd
        |> Seq.map parseScannerBeacons
    groups

// There are only 24 valid orientations, but I can't seem to figure out how to 
// identify the invalid ones. So 
// Cutting this down to 24 would be the easiest optimization to do in terms of code needed
let transformBeacon v beacon =
    let (x,y,z) = beacon
    match v with 
    | 0 ->  ( x,  y,  z)
    | 1 ->  ( x,  y, -z)
    | 2 ->  ( x, -y,  z)
    | 3 ->  ( x, -y, -z)
    | 4 ->  (-x,  y,  z)
    | 5 ->  (-x,  y, -z)
    | 6 ->  (-x, -y,  z)
    | 7 ->  (-x, -y, -z)
    | 8 ->  ( y,  x,  z)
    | 9 ->  ( y,  x, -z)
    | 10 -> ( y, -x,  z)
    | 11 -> ( y, -x, -z)
    | 12 -> (-y,  x,  z)
    | 13 -> (-y,  x, -z)
    | 14 -> (-y, -x,  z)
    | 15 -> (-y, -x, -z)
    | 16 -> ( z,  y,  x)
    | 17 -> ( z,  y, -x)
    | 18 -> ( z, -y,  x)
    | 19 -> ( z, -y, -x)
    | 20 -> (-z,  y,  x)
    | 21 -> (-z,  y, -x)
    | 22 -> (-z, -y,  x)
    | 23 -> (-z, -y, -x)

    | 24 ->  ( x,  z,  y)
    | 25 ->  ( x,  z, -y)
    | 26 ->  ( x, -z,  y)
    | 27 ->  ( x, -z, -y)
    | 28 ->  (-x,  z,  y)
    | 29 ->  (-x,  z, -y)
    | 30 ->  (-x, -z,  y)
    | 31 ->  (-x, -z, -y)

    | 32 ->  ( y,  z,  x)
    | 33 ->  ( y,  z, -x)
    | 34 -> ( y, -z,  x)
    | 35 -> ( y, -z, -x)
    | 36 -> (-y,  z,  x)
    | 37 -> (-y,  z, -x)
    | 38 -> (-y, -z,  x)
    | 39 -> (-y, -z, -x)

    | 40 -> ( z,  x,  y)
    | 41 -> ( z,  x, -y)
    | 42 -> ( z, -x,  y)
    | 43 -> ( z, -x, -y)
    | 44 -> (-z,  x,  y)
    | 45 -> (-z,  x, -y)
    | 46 -> (-z, -x,  y)
    | 47 -> (-z, -x, -y)
let tranformBeaconSet beacons v =
    if v = 0 then beacons
    else beacons |> Set.map (transformBeacon v)

let findOverlap (first:Set<Beacon>) (second:Set<Beacon>) =
    let isMatching ((x, y, z):Beacon) ((x1,y1,z1):Beacon) ((xo, yo, zo):int*int*int) =
        x = (x1 + xo) && y = (y1 + yo) && z = (z1 + zo)

    // We could have calculated the distance between points in a grid and compared those first.
    // No matter the orientation of a scanner, the distances between its points will be the same
    let distance ((x,y,z):Beacon) ((x1, y1, z1):Beacon) =
        sqrt (double ((x1 - x) * (x1 - x) + (y1-y) * (y1-y) + (z1-z) * (z1-z)))

    let calcOffset ((x,y,z):Beacon) ((x1, y1, z1)):Beacon =
        (x - x1, y - y1, z - z1)
    let has12Matching first second ((xo,yo,zo):int*int*int) =
        let offsetSet = second |> Set.map (fun (x,y,z) -> Beacon(x + xo, y + yo, z + zo))
        let count = Set.intersect first offsetSet |> Set.count
        (count > 11, offsetSet)

    let rec hasEnoughPairs first transformed pairs =
        match pairs with
        | head::tail -> 
            let offset = calcOffset (fst head) (snd head)
            let (has12, ofsetSet) = has12Matching first transformed offset
            if has12 then
                (true, ofsetSet, offset)
            else 
                hasEnoughPairs first transformed tail
        | [] -> (false, Set.empty, (0,0,0))
      
    let rec hasMatch iteration =
        if iteration > 47 then  
            (false, Set.empty, (-1,-1,-1))
        else
            let transformedSet = tranformBeaconSet second iteration
            let pairs = List.allPairs (first |> List.ofSeq) (transformedSet |> List.ofSeq)
            let (hasEnoughPairs, correctSet, offset) = hasEnoughPairs first transformedSet pairs

            if hasEnoughPairs then
                (true, correctSet, offset)
            else
                hasMatch (iteration + 1)

    let (hasMatch, set, beaconPos) = hasMatch 0
        
    (hasMatch, set, beaconPos)

// Takes a couple of minutes to execute
let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input |> List.ofSeq

    let mutable locatedBeacons = [  (parsed.Head, (0,0,0)) ]
    let mutable remaining = parsed |> List.tail

    let rec matchSingle current (matched:(Set<Beacon>*(int*int*int)) list) =
        match matched with
        | head::tail -> 
            let (hasMatch, newSet, position) = findOverlap (fst head) current
            if hasMatch then
                (hasMatch, newSet, position)
            else
                matchSingle current tail
        | [] -> (false, Set.empty, (0,0,0))

    let rec matchAll remaining (matched:(Set<Beacon>*(int*int*int)) list) (notMatched:Set<Beacon> list) =
        match remaining with
        | head::tail -> 
            let (hasMatch, newSet, position) = matchSingle head matched
            if hasMatch then
                matchAll tail ((newSet, position)::matched) notMatched
            else
                matchAll tail matched (head::notMatched)
        | [] -> (matched, notMatched)
    
    while remaining.Length > 0 do
        let (m, n) = matchAll remaining locatedBeacons []
        locatedBeacons <- m
        remaining <- n
        printfn "%i found %i left" locatedBeacons.Length remaining.Length 

    // Merge all the corrected sets so overlapping points will be merged and count them
    let part1 = 
        locatedBeacons 
        |> List.map fst
        |> List.fold (fun agg curr -> Set.union agg curr) Set.empty
        |> Set.count

    // When we adjusted the beacon grids we also automatically found the scanner position.
    // We just need to take each position, calculate the distance between all combinations
    // and find the largest
    let manhattanDistance (x1,y1,z1) (x2,y2,z2) =
        (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))

    let distances =
        locatedBeacons
        |> Seq.map snd
    let pairs = 
        Seq.allPairs distances distances
        |> Seq.map (fun (fst, snd) -> manhattanDistance fst snd)
        |> Seq.max

    let part2 = pairs

    part1.ToString(), part2.ToString()