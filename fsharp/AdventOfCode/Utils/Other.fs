module Other

open System

type Point = 
    { X: int; Y: int }
    member this.tuple() = (this.X, this.Y)
    static member create(tuple: int * int) = 
        { X = fst tuple; Y = snd tuple }


let grid2dAllNeighbors = [ 
    (-1,-1); (0,-1); (1,-1);
    (-1, 0);         (1, 0);
    (-1, 1); (0, 1); (1, 1)
]

let grid2dStraightNeighbors = [ 
             (0,-1); 
    (-1, 0);         (1, 0);
             (0, 1)
]

let getNeighbors neighborMap loc grid =
    let xLength = grid |> Array2D.length1
    let yLength = grid |> Array2D.length2

    neighborMap 
    |> List.map (fun (x,y) -> ((fst loc) + x, (snd loc) + y))
    |> List.filter (fun (x,y) -> x >= 0 && y >= 0)
    |> List.filter (fun (x,y) -> x < xLength && y < yLength)