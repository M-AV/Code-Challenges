module _2023_10

open InputProvider
open Calculations
open Parsing
open Other
open System
open Xunit
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type TupleExtensions () = 
    [<Extension>] static member X((a,b)) = a
    [<Extension>] static member Y((a,b)) = b

let parseInput (input : string seq) = 
    let arr = parseArray2d input
        
    let start = find2d 'S' arr            

    arr, start.Value

let nextPos (grid:char[,]) (x:int,y:int) =
    match grid[x,y] with
    | '|' -> ((x, y-1), (x,y+1))
    | '-' -> ((x-1, y), (x+1,y))
    | 'L' -> ((x, y-1), (x+1,y))
    | 'J' -> ((x-1, y), (x,y-1))
    | '7' -> ((x-1, y), (x,y+1))
    | 'F' -> ((x+1, y), (x,y+1))
    | 'S' -> 
        let xLength = grid |> Array2D.length1
        let yLength = grid |> Array2D.length2

        let s = 
            grid2dStraightNeighbors 
            |> List.map (fun (xn,yn) -> x + xn, y + yn)
            |> List.filter (fun (xn, yn) -> xn >= 0 && yn >= 0)
            |> List.filter (fun (xn, yn) -> xn < xLength && yn < yLength)
            |> List.filter (fun (xn, yn) -> grid[xn, yn] <> '.')
            |> List.filter (fun (xn, yn) -> 
                let bannedChars = 
                    if xn < x then
                        ['|'; 'J'; '7']
                    else if xn > x then
                        ['F'; '|'; 'L']
                    else if yn < y then
                        ['L'; '-'; 'J']
                    else
                        ['F'; '-'; '7']
                        
                bannedChars |> List.contains grid[xn, yn] |> not
                )
            |> Array.ofList
            
        s[0],s[1]

let up = (0, -1)
let down = (0, +1)
let right = (1, 0)
let left = (-1, 0)

let findNextStep (trackingGrid:int array2d) arr (xi, yi) =
    let (p1, p2) = nextPos arr (xi, yi)
    if trackingGrid[fst p1, snd p1] = -1 then
        p1
    else 
        p2

// Start with the first two pipes reaching S, then recursively 
// find the next one of each and progress pairwise through until we reach the farthest point
let rec bfs (trackingGrid:int array2d) grid (queue:Queue<(int32*int32)*(int32 * int32)>) count =
    let (first,second) = queue.Dequeue()

    if trackingGrid[fst first, snd first] <> -1 then
        trackingGrid[fst first, snd first]
    else if trackingGrid[fst second, snd second] <> -1 then
        trackingGrid[fst second, snd second]
    else
        trackingGrid[fst first, snd first] <- count
        trackingGrid[fst second, snd second] <- count

        let next1 = findNextStep trackingGrid grid first
        let next2 = findNextStep trackingGrid grid second

        queue.Enqueue((next1, next2))

        bfs trackingGrid grid queue (count+1)

let part1 (arr:char[,]) (x,y) =
    let trackingGrid = Array2D.create (Array2D.length1 arr) (Array2D.length2 arr) -1
    trackingGrid[x,y] <- 0

    let nextPositions = nextPos arr (x,y)

    let q = [nextPositions] |> Queue<(int32 * int32) * (int32 * int32)>

    bfs trackingGrid arr q 1 |> ignore

    let mutable res = 0
    trackingGrid |> Array2D.iter (fun x -> res <- max res x)

    trackingGrid, res

let rec findStartValue (grid:char[,]) startPos firstN secondN : char =
    // Haven't really tested this, but it works for my input..
    if (fst firstN < fst startPos) && (snd secondN < snd startPos) then 'J'
    else if (fst firstN < fst startPos) && (fst secondN > fst startPos) then '-'
    else if (fst firstN < fst startPos) && (snd secondN > snd startPos) then '7'
    else if (fst firstN = fst startPos) && (fst secondN = fst startPos) then '|'
    else if (fst firstN > fst startPos) && (snd secondN < snd startPos) then 'L'
    else if (fst firstN > fst startPos) && (snd secondN > snd startPos) then 'F'
    else
        findStartValue grid startPos secondN firstN

/// Tried a few things in December and kept adding to the original idea and eventually things got a bit bloated.
/// Went on Christmas vacation and got back to it 4 months later, not knowing the details anymore. At that point 
/// I just wanted to finish the task, so didn't spend much time trying to improve the code.
/// 
/// The basic idea is this:
/// 1. Surround the grid with '.' and mark all points that are obviously reachable from the edge of the grid.
/// 2. For each remaining '.', find the nearest edge and traverse the edge, while keeping track of which side 
///    you are on and which places you have visited.
/// 3. When traversing, if you reach an point that is known to be reachable from the edge, mark all visited points
///    as reachable as well.
/// 4. If you reach your starting point without having seen such a point, that means you are in a loop and all 
///    points inside are enclosed and we mark them as such.
/// 5. Count all points marked as enclosed.
let part2 (trackingGrid:int array2d) (grid:char[,]) startPos=
    // Replace S with correct pipe - Needed so we can follow the pipe through S if necessary
    let (firstN, secondN) = nextPos grid startPos
    grid[fst startPos, snd startPos] <- findStartValue grid startPos firstN secondN

    // Add padding to make sure all outer spaces are connected
    let trackingGrid = addPaddingToExisting_int -1 trackingGrid
    let grid = addPaddingToExisting_char '.' grid

    // Remove all unused pipes - Those fields have to count as well
    trackingGrid |> Array2D.iteri (fun x y _ -> 
        grid[x,y] <- if trackingGrid[x,y] = -1 then '.' else grid[x,y]) |> ignore

    let rec floodSpace remaining char =
        match remaining with
        | [] -> 0
        | head::rest -> 
            let neighbors = (getNeighbors grid2dAllNeighbors head grid) |> List.filter (fun (x,y) -> grid[x,y] = '.')
            grid[fst head, snd head] <- char

            floodSpace (neighbors@rest) char

    // Mark all spaces around the edge. Allows us to avoid checking all of those and we can ignore the edge-cases
    floodSpace [(0,0)] '!' |> ignore


    // From a line and a . next to the edge, follow the line until we see a ! or a ?
    // If we see a ! we are out and can mark all the locations as !.
    // If we wrap around and see a ?, we should be contained inside the container
    let followEdge edgeLoc ourLoc  =
        let nextLoc (x,y) (xd, yd) =
            (x + xd), y+yd
        let nextLoc2 (x,y) (x1, y1) (x2, y2) = 
            (x + x1, y+y1), (x + x2, y+y2)
        let markVisited (x,y) (list:(int * int) list) =
            if not list.IsEmpty && (x,y) = list.Head then
                grid[x,y] <- '?'
                list
            else if grid[x, y] = '.' then
                grid[x,y] <- '?'
                (x,y)::list
            else
                list

        let back (x,y) =
            (-x, -y)

        let rec followEdgeInner edgeLoc ourLoc (prevEdgeLoc:int*int) direction (visited:(int*int) list) (visitedEdges:(int*int) Set) (side:(int*int)): ((int * int) list * bool) =
            let sideLoc = nextLoc edgeLoc side

            //let nextEdge = nextLoc edgeLoc direction
            //printfn "Next: %A, Edge: %A, Prev: %A, Our: %A" grid[fst nextEdge, snd nextEdge] grid[fst edgeLoc, snd edgeLoc] grid[fst prevEdgeLoc, snd prevEdgeLoc] grid[fst ourLoc, snd ourLoc]
            //printfn "Dir: %A" direction
            //printGrid2d_color [nextLoc edgeLoc direction] [edgeLoc] [prevEdgeLoc] [ourLoc] grid


            if visitedEdges |> Set.contains edgeLoc then
                visited, true
            else if (grid[fst edgeLoc, snd edgeLoc] = '|' || grid[fst edgeLoc, snd edgeLoc] = '-') && grid[fst sideLoc, snd sideLoc] = '!' then
                visited, false
            else
                let updatedVisitedEdges = (visitedEdges |> Set.add edgeLoc)

                match grid[fst edgeLoc, snd edgeLoc] with
                | '|' | '-' ->
                    let nextXY = (nextLoc2 direction edgeLoc ourLoc)
                    let newVisited = markVisited ourLoc visited
                    followEdgeInner (fst nextXY) (snd nextXY) edgeLoc direction newVisited updatedVisitedEdges side
                | 'L' -> // Down, left possible (on each side of the line)
                    let upEdge = nextLoc edgeLoc (0, -1)
                    let rightEdge = nextLoc edgeLoc (1, 0)
                    let prevDirection = direction
                    let direction = if prevEdgeLoc = upEdge then (1, 0) else (0, -1)
                    
                    // On the outside
                    if (prevEdgeLoc = upEdge && (fst ourLoc) < (fst edgeLoc)) ||
                       (prevEdgeLoc = rightEdge && (snd edgeLoc < snd ourLoc)) then 

                       // "Walk" around the corner, to the our next position
                       let step1 = nextLoc prevDirection ourLoc
                       let mutable newVisited = markVisited step1 visited
                       let step2 = nextLoc direction step1
                       newVisited <- markVisited step2 newVisited
                       let step3 = nextLoc direction step2
                       newVisited <- markVisited step3 newVisited

                       let newOurLoc = step3
                       let newEdge = nextLoc direction edgeLoc
                       let side = if prevEdgeLoc = upEdge then down else left
                       followEdgeInner newEdge newOurLoc edgeLoc direction newVisited updatedVisitedEdges side
                    else // Inside
                       let newOurLoc = nextLoc ourLoc (back prevDirection)
                       let side = if prevEdgeLoc = upEdge then up else right
                       followEdgeInner (nextLoc edgeLoc direction) newOurLoc edgeLoc direction visited updatedVisitedEdges side
                   
                | 'J' -> // Down, Right
                    let upEdge = nextLoc edgeLoc (0, -1)
                    let leftEdge = nextLoc edgeLoc (-1, 0)
                    let prevDirection = direction
                    let direction = if prevEdgeLoc = upEdge then (-1, 0) else (0, -1)

                    // On the outside
                    if (prevEdgeLoc = upEdge && (fst edgeLoc) < (fst ourLoc)) ||
                       (prevEdgeLoc = leftEdge && (snd edgeLoc < snd ourLoc)) then 

                       // "Walk" around the corner, to the our next position
                       let step1 = nextLoc prevDirection ourLoc
                       let mutable newVisited = markVisited step1 visited
                       let step2 = nextLoc direction step1
                       newVisited <- markVisited step2 newVisited
                       let step3 = nextLoc direction step2
                       newVisited <- markVisited step3 newVisited

                       let newOurLoc = step3
                       let newEdge = nextLoc direction edgeLoc
                       let side = if prevEdgeLoc = upEdge then down else right
                       followEdgeInner newEdge newOurLoc edgeLoc direction newVisited updatedVisitedEdges side
                    else // Inside
                        let newOurLoc = nextLoc ourLoc (back prevDirection)
                        let side = if prevEdgeLoc = upEdge then up else left
                        followEdgeInner (nextLoc edgeLoc direction) newOurLoc edgeLoc direction visited updatedVisitedEdges side
                | '7' -> // Up, Right
                    let prevDirection = direction
                    let direction = if (snd direction) = -1 then (-1, 0) else (0, 1) // If we were going up, we now go left and vice versa

                    if edgeLoc.X() < ourLoc.X() || ourLoc.Y() < edgeLoc.Y() then // On the outside

                       // "Walk" around the corner, to the our next position
                       let step1 = nextLoc prevDirection ourLoc
                       let mutable newVisited = markVisited step1 visited
                       let step2 = nextLoc direction step1
                       newVisited <- markVisited step2 newVisited
                       let step3 = nextLoc direction step2
                       newVisited <- markVisited step3 newVisited

                       let newOurLoc = step3
                       let newEdge = nextLoc direction edgeLoc
                       let side = if (snd direction) = -1 then up else right
                       followEdgeInner newEdge newOurLoc edgeLoc direction newVisited updatedVisitedEdges side
                    else // Inside
                        let newOurLoc = nextLoc ourLoc (back prevDirection)
                        let side = if (snd direction) = -1 then down else left

                        followEdgeInner (nextLoc edgeLoc direction) newOurLoc edgeLoc direction visited updatedVisitedEdges side
                       
                | 'F' -> // Up, Left
                    let downEdge = nextLoc edgeLoc (0, 1)
                    let rightEdge = nextLoc edgeLoc (1, 0)
                    let prevDirection = direction

                    let direction = if prevEdgeLoc = downEdge then (1, 0) else (0, 1)

                    // On the outside
                    if (prevEdgeLoc = downEdge && (fst ourLoc) < (fst edgeLoc)) ||
                       (prevEdgeLoc = rightEdge && (snd ourLoc < snd edgeLoc)) then 

                       // "Walk" around the corner, to the our next position
                       let step1 = nextLoc prevDirection ourLoc
                       let mutable newVisited = markVisited step1 visited
                       let step2 = nextLoc direction step1
                       newVisited <- markVisited step2 newVisited
                       let step3 = nextLoc direction step2
                       newVisited <- markVisited step3 newVisited

                       let newOurLoc = step3
                       let newEdge = nextLoc direction edgeLoc
                       let side = if prevEdgeLoc = downEdge then up else left
                       followEdgeInner newEdge newOurLoc edgeLoc direction newVisited updatedVisitedEdges side
                    else // Inside
                        let newOurLoc = nextLoc ourLoc (back prevDirection)
                        let side = if prevEdgeLoc = downEdge then down else right
                        followEdgeInner (nextLoc edgeLoc direction) newOurLoc edgeLoc direction visited updatedVisitedEdges side

        match grid[fst edgeLoc, snd edgeLoc] with
        | 'L' | 'F' | '-' -> 
            let nextEdge = nextLoc (1, 0) edgeLoc
            let nextOurLoc = nextLoc (1,0) ourLoc
            followEdgeInner nextEdge nextOurLoc edgeLoc (1,0) [ourLoc] Set.empty left
        | 'J' | '7' -> 
            let nextEdge = nextLoc (-1, 0) edgeLoc
            let nextOurLoc = nextLoc (-1,0) ourLoc
            followEdgeInner nextEdge nextOurLoc edgeLoc (-1,0) [ourLoc] Set.empty right
        | '|' -> 
            let nextEdge = nextLoc (0, 1) edgeLoc
            let nextOurLoc = nextLoc (0, 1) ourLoc
            followEdgeInner nextEdge nextOurLoc edgeLoc (0,1) [ourLoc] Set.empty down

    let processSpace x y =
        let rec markSpace ourLoc remaining (visited:(int32*int32) list) =
            match remaining with
            | head::rest -> 
                let item = grid[fst head, snd head]
                //printfn "Loc: %A   %A    %A   %A" head item ourLoc grid[fst ourLoc, snd ourLoc]
                if item = '&' then
                    markSpace head rest visited
                else if item = '.' then
                    markSpace head rest (head::visited)
                else if item = '?' then
                    visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '?')
                else if item = '!' then
                    visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '!')
                else 
                    let visited, surrounded = followEdge head ourLoc
                    if surrounded then
                        visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '&')
                        visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '&')
                        floodSpace visited '&' |> ignore
                    else 
                        visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '!')
                        visited |> List.iter (fun loc -> grid[fst loc, snd loc] <- '!')
                        floodSpace visited '!' |> ignore
                    
                    //markSpace head rest visited
            | [] -> 
                ()


        let neighbors = getNeighbors grid2dStraightNeighbors (x,y) grid
        markSpace (x,y) neighbors []

    Array2D.iteri (fun x y v -> 
        if v = '.' then
            processSpace x y |> ignore
    ) grid

    let mutable count = 0
    grid |> Array2D.iter (fun v -> if v = '&' then count <- count + 1)

    // printGrid2d grid

    count

let execute (input : string seq) =
    let parsed = parseInput input

    let grid, part1Res = part1 (fst parsed) (snd parsed)

    let part1 = part1Res

    let part2 = part2 grid (fst parsed) (snd parsed)

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "10" |> Async.RunSynchronously)
    Assert.Equal("6867", part1)
    Assert.Equal("595", part2)