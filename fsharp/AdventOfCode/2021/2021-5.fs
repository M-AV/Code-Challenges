module _2021_5

open System
open System.Linq
open System.Diagnostics

// Task 1: Find number of points were a horizontal/ vertical line overlaps
// Task 2: 
type Point = 
    struct 
        val X: int
        val Y: int
        new (x, y) = { X = x; Y = y}

        override this.ToString() =
            "(" + this.X.ToString() + ", " + this.Y.ToString() + ")"
    end

let parseLine (line: string) =
    line.Split(" -> ", StringSplitOptions.RemoveEmptyEntries) |>
    Array.map (fun (x) -> x.Split(',') |> 
                          Seq.pairwise |> 
                          Seq.map (fun (x,y) -> new Point(int x,int y)) |> 
                          Seq.head)
let parseLine2 (line: string) =
    let [x; y] = line.Split(" -> ", StringSplitOptions.RemoveEmptyEntries) |>
                    List.ofArray |>
                    List.map (fun (x) -> x.Split(',') |> 
                                            Seq.pairwise |> 
                                            Seq.map (fun (x,y) -> new Point(int x,int y)) |> 
                                            Seq.head)
    (x, y)

let isHorizontal (line:Point[]) = line[0].Y = line[1].Y
let isVertical (line:Point[]) = line[0].X = line[1].X
let minX (line:Point[]) = min line[0].X line[1].X
let minY (line:Point[]) = min line[0].Y line[1].Y
let maxX (line:Point[]) = max line[0].X line[1].X
let maxY (line:Point[]) = max line[0].Y line[1].Y

let minByX (line:Point[]) = Array.minBy (fun (x:Point) -> x.X)
let maxByX (line:Point[]) = Array.minBy (fun (x:Point) -> x.X)
let minByY (line:Point[]) = Array.minBy (fun (x:Point) -> x.Y)
let maxByY (line:Point[]) = Array.minBy (fun (x:Point) -> x.Y)

// I originally wanted to do a recursive function that checked every line against every other line
// which was super inefficient. It didn't give me the correct result, so instead of debugging it 
// I tried my luck with a Map and a single iteration of each point of each line... It works.. But 
// this is also super inefficient, even though I would have assumed otherwise.. Hmm.

let addLineToMap (line:Point[], map:Map<Point, int>) =
    let increment x = 
        match x with 
        | Some x -> Some(x + 1)
        | None -> Some(1)
    
    let rec addPoints (points, map:Map<Point, int>) =
        match points with 
        | x when Seq.isEmpty x -> map
        | _ -> addPoints ((Seq.tail points), (map.Change((Seq.head points), increment)))

    let generatePoints line =
        match line with 
        | x when isHorizontal x || isVertical x -> 
            [ for x in (minX line) .. (maxX line) do 
                for y in (minY line) .. (maxY line) -> new Point(x,y)]
        | _ -> List.empty

    let result = addPoints ((generatePoints line), map)

    result
        
let solvePart1WithMap (lines:Point[] seq) =
    let rec addLinesToMap (lines:Point[] seq, map) =
        match lines with 
        | x when Seq.isEmpty x -> map
        | _ -> addLinesToMap ((Seq.tail lines), (addLineToMap ((Seq.head lines), map)))

    let positionCount = addLinesToMap (lines, (Map []))

    printfn "Counting.."

    positionCount.Values |> Seq.filter (fun x -> x > 1) |> Seq.length

// In my pursuit of knowledge I found this solution https://github.com/jovaneyck/advent-of-code-2021/blob/main/day%2005/part1.fsx
// which is using lots of stuff I have no idea how works.. 
// The below solution is that one which has the performance I would expect.

let solvePart1WithCountBy (lines:(Point * Point) seq) =
    let generatePoints (first:Point, second:Point) =
        let [ min_x; max_x ] = [first.X; second.X] |> List.sort
        let [ min_y; max_y ] = [first.Y; second.Y] |> List.sort

        let points = [ for x in min_x .. max_x do 
                        for y in min_y .. max_y -> (x,y)] // This works because either x or y will be the same. Wont work for diagonal
        
        points
    
    lines |> 
        Seq.collect generatePoints |> 
        Seq.countBy id |> 
        Seq.filter (fun (_, count) -> count > 1) |>
        Seq.length

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = input |> Seq.map parseLine

    // Part 1
    let part1Lines = 
        parsed |>
        Seq.filter (fun x -> x[0].X = x[1].X || x[0].Y = x[1].Y)
    let overlapCount = solvePart1WithMap part1Lines

    //let parsed = input |> Seq.map parseLine2
    //let part1Lines = 
    //    parsed |>
    //    Seq.filter (fun x -> (fst x).X = (snd x).X || (fst x).Y = (snd x).Y)
    //let overlapCount = solvePart1WithCountBy part1Lines

    let part1 = overlapCount

    let part2 = "N/A"

    part1.ToString(), part2.ToString()