module _2021_23

// Task 1: Find minimum cost to put all in the correct basket
// Task 2: 

open System


type Hallway = Map<int, char option>

type Node = 
    {
        Item: char option
    }
    member x.isEmpty() =
        match x.Item with
        | Some _ -> true
        | None -> false

type Room = char * char list * int
type GameState = Map<int, Node> * Room list

let printGameState ((hallway, rooms):GameState) = 
    for i in hallway do
        printf "%c" (if i.Value.Item.IsSome then i.Value.Item.Value else '.')
    printfn ""
    printf "#"
    for (ch, room, size) in rooms do
        printf "#"
        printf "%c" (if room.IsEmpty || room.Length = 1 then '.' else room.Head)
    printfn "##"
    printf "#"
    for (ch, room, size) in rooms do
        printf "#"
        printf "%c" (if room.Length = 1 then room.Head else if room.Length < 2 then '.' else room[1])
    printfn "##"

let moveCost amphipod = 
    match amphipod with
    | 'A' -> 1
    | 'B' -> 10
    | 'C' -> 100
    | 'D' -> 1000
    | _ -> failwith "Invalid amphipod"
let roomHallwayIdx room =
    match room with
    | 'A' -> 2
    | 'B' -> 4
    | 'C' -> 6
    | 'D' -> 8
    | _ -> failwith "Invalid room"
let roomIdx room =
    match room with
    | 'A' -> 0
    | 'B' -> 1
    | 'C' -> 2
    | 'D' -> 3
    | _ -> failwith "Invalid room"
let isEntry roomHallwayIdx =
    match roomHallwayIdx with
    | 2 | 4 | 6 | 8 -> true
    | _ -> false

let parseInput (input: string seq) =
    let filtered = 
        input
        |> Seq.filter (fun x -> x |> Seq.exists (fun char -> char <> '#'))
    let graph =
        filtered
        |> Seq.head
        |> Seq.filter (fun char -> char <> '#') 
        |> Array.ofSeq

    let map =
        graph
        |> Array.indexed
        |> Array.map (fun (idx, _) -> (idx, { Item = None }))
        |> Map.ofArray
    let buckets =
        filtered
        |> Seq.tail
        |> Seq.rev
        |> Seq.map (fun line -> line |> Seq.filter (fun char -> char <> '#') |> Array.ofSeq)
        |> Seq.fold (fun (agg:char list[]) cur -> 
            agg[0] <- cur[0]::agg[0]
            agg[1] <- cur[1]::agg[1]
            agg[2] <- cur[2]::agg[2]
            agg[3] <- cur[3]::agg[3]
            agg) ([|List.empty;List.empty;List.empty;List.empty|])
        |> Seq.indexed
        |> Seq.map (fun (idx, arr) -> 
            match idx with
            | 0 -> Room('A', arr, 2)
            | 1 -> Room('B', arr, 2)
            | 2 -> Room('C', arr, 2)
            | 3 -> Room('D', arr, 2))
        |> List.ofSeq

    GameState(map, buckets)

let solveWithLowestCost (input:GameState) =
    let roomIsSolved ((char, room, size):Room) = 
        room.Length = size && (room |> List.forall (fun x -> x = char))

    let isSolved ((hallway, rooms):GameState) = rooms |> Seq.forall roomIsSolved
    let canMoveHomeFromHallway (idx, char) ((hallway, rooms):GameState) =
        let evaluateRoom ((char, room, size):Room) = 
            let charCost = moveCost char
            let moves = size - room.Length
            let totalCost = charCost * moves
            if room.IsEmpty then (totalCost, true)
            else if room |> List.forall (fun x -> x = char) then (totalCost, true)
            else (Int32.MaxValue, false)
        let evaluatePath char charIdx = 
            let hallwayIdx = roomHallwayIdx char
            let charCost = moveCost char
            let path = [ (min hallwayIdx charIdx) .. (max hallwayIdx charIdx) ]
            let isClear = 
                path
                |> Seq.filter (fun x -> x <> charIdx)
                |> Seq.forall (fun x -> hallway[x].Item.IsNone)
            let cost = (path.Length - 1) * charCost
            (cost, isClear)

        let (char, room, sz) = rooms[roomIdx char]
        let (roomCost, roomAvail) = evaluateRoom rooms[roomIdx char]
        let (pathCost, pathClear) = evaluatePath char idx

        (roomCost + pathCost, roomAvail && pathClear)
    
    let moveHome idx ((hallway, rooms):GameState) =
        let value = hallway[idx]
        let roomIdx = roomIdx value.Item.Value
        let newRooms = 
            rooms 
            |> List.indexed
            |> List.map (fun (idx, (char, room, sz)) -> 
                match idx with
                | x when roomIdx = x -> (char, value.Item.Value::room, sz)
                | _ -> (char, room, sz))
        let newHallway = hallway |> Map.remove idx |> Map.add idx { Item = None }

        (GameState(newHallway, newRooms))
    let moveOut roomChar ((hallway, rooms):GameState) : (GameState * int) list =
        // Find all places we can put the char if it moves out and create all gamestates
        let roomEntry = roomHallwayIdx roomChar
        let roomIdx = roomIdx roomChar
        let (ch, room, sz) = rooms[roomIdx]
        let movesToReachHallway = sz - room.Length + 1

        if room.IsEmpty || room |> List.forall (fun x -> x = ch) then
            []
        else
            let movedChar = room.Head
            let leftMoves = [ 0 .. roomEntry - 1 ] |> List.rev |> List.takeWhile (fun x -> hallway[x].Item.IsNone)
            let rightMoves = [roomEntry + 1 .. hallway.Count - 1] |> List.takeWhile (fun x -> hallway[x].Item.IsNone)
            
            let moves = 
                (leftMoves @ rightMoves)
                |> Seq.filter (not << isEntry)
                |> Seq.map (fun hallwayIdx -> 
                    let newRooms = 
                        rooms 
                        |> List.indexed
                        |> List.map (fun (idx, (char, room, sz)) -> 
                            match idx with
                            | x when roomIdx = x -> (char, room.Tail, sz)
                            | _ -> (char, room, sz))
                    let newHallway = hallway |> Map.remove hallwayIdx |> Map.add hallwayIdx ( { Item = Some(movedChar) })
                    let cost = (max hallwayIdx roomEntry) - (min hallwayIdx roomEntry) + movesToReachHallway
                    (GameState(newHallway, newRooms), cost * (moveCost movedChar)))
                |> List.ofSeq
                
            moves
        
    let getAvailableMoves ((hallway, rooms):GameState) currentCost : (GameState * int) list =
        let moveHomeStates = 
            hallway
            |> Seq.filter (fun x -> Option.isSome x.Value.Item)
            |> Seq.map (fun x -> 
                let (cost, possible) = canMoveHomeFromHallway (x.Key, x.Value.Item.Value) (hallway, rooms)
                if possible then
                    Some(((moveHome x.Key (hallway, rooms))), cost + currentCost)
                else 
                    None)
            |> Seq.choose (fun x -> x)
            |> List.ofSeq

        let moveOutStates = 
            rooms
            |> Seq.map (fun (char, room, sz) -> moveOut char (hallway, rooms))
            |> Seq.collect id
            |> Seq.map (fun (game, score) -> (game, score + currentCost))
            |> List.ofSeq

        moveHomeStates @ moveOutStates

    let mutable minCost = Int32.MaxValue

    // Don't know when this application stops. We're processing lots of duplicate states, 
    // but when I add a map to keep track of previous states I end up with the wrong result.. 
    // It finds the correct result fairly quickly, but keeps running. :/
    let rec solve state currentCost = 
        let availableMoves = getAvailableMoves state currentCost |> List.filter (fun (_, cost) -> cost < minCost)

        let cost = 
            match state with
            | x when isSolved x -> 
                if currentCost < minCost then
                    minCost <- currentCost
                    printfn "New score: %i" minCost
                currentCost
            | x when List.isEmpty availableMoves -> Int32.MaxValue // Unsolvable
            | x -> 
                if currentCost > minCost then
                    Int32.MaxValue
                else 
                    availableMoves
                    |> Seq.sortBy (fun (_, cost) -> cost)
                    |> Seq.map (fun (state, cost) -> solve state cost)
                    |> Seq.fold (fun agg curr -> if curr < agg then curr else agg) Int32.MaxValue
        cost
    solve input 0
        

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    printfn "%A" parsed

    // Currently, you will have to comment out part 1 to see part 2 as I don't know when the program
    // finishes.. (maybe never?) However, it does print out the correct result after a few seconds

    let part1 = solveWithLowestCost parsed

    let (hallway, rooms) = parsed

    let updateRoom ((char, room, sz):Room) =
        match char with
        | 'A' -> Room(char, room.Head::'D'::'D'::[ room[1] ], 4)
        | 'B' -> Room(char, room.Head::'C'::'B'::[ room[1] ], 4)
        | 'C' -> Room(char, room.Head::'B'::'A'::[ room[1] ], 4)
        | 'D' -> Room(char, room.Head::'A'::'C'::[ room[1] ], 4)

    let part2Input = (hallway, rooms |> List.map updateRoom)
        

    let part2 = solveWithLowestCost part2Input

    part1.ToString(), part2.ToString()