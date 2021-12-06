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
        let fdsfs = match x with 
                    | Some x -> Some(x + 1)
                    | None -> Some(1)
        fdsfs
    
    

    let rec addPoints (points, map:Map<Point, int>) =
        //let timer = new Stopwatch()
        //timer.Start()
        //printfn "   Points: %A" points
        let res = match points with 
                    | x when Seq.isEmpty x -> map
                    | _ -> addPoints ((Seq.tail points), (map.Change((Seq.head points), increment)))
        //timer.Stop()
        //printfn "%A" timer.Elapsed
        res

    let generatePoints line =
        let res = match line with 
                    | x when isHorizontal x -> 
                        Enumerable.Range(minX line, (maxX line - minX line + 1)) |> 
                        Seq.map (fun x -> new Point(line[0].Y, x))
                    | x when isVertical x -> 
                        Enumerable.Range(minY line, (maxY line - minY line + 1)) |> 
                        Seq.map (fun y -> new Point(y,line[0].X))
                    | _ -> Seq.empty
        res

    //printfn "LINE: %A" line

    let timer = new Stopwatch()
    timer.Start()
    let result = addPoints ((generatePoints line), map)
    
    timer.Stop()
    printfn "%A" timer.Elapsed

    result
        
let solvePart1WithMap (lines:Point[] seq) =
    let rec addLinesToMap (lines:Point[] seq, map) =
        //printfn "Adding %A to map" (Seq.head lines)
        match lines with 
        | x when Seq.isEmpty x -> map
        | _ -> addLinesToMap ((Seq.tail lines), (addLineToMap ((Seq.head lines), map)))

    let positionCount = addLinesToMap (lines, (Map []))

    printfn "Counting.."

    positionCount.Values |> Seq.filter (fun x -> x > 1) |> Seq.length


let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = input |> Seq.map parseLine

    // Part 1
    let part1Lines = 
        parsed |>
        Seq.filter (fun x -> x[0].X = x[1].X || x[0].Y = x[1].Y)

    let overlapCount = solvePart1WithMap part1Lines
    //let overlapCount = countAllOverlapping part1Lines

    let part1 = overlapCount

    let part2 = "N/A"

    part1.ToString(), part2.ToString()