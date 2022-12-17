module Calculations

open System

// Useful for calculating a distance through a grid
// https://en.wikipedia.org/wiki/Taxicab_geometry
let manhattanDistance2D (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2)
let manhattanDistance3D (x1, y1, z1) (x2, y2, z2) =
    abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)

let array2dMax input =
    let mutable max = Int32.MinValue

    input |> Array2D.iter (fun x -> 
        if (x > max) then
            max <- x);

    max

// From here https://stackoverflow.com/a/32830039/1401257
// Works by taking one element and calculating all subsets without that element, 
// then duplicating all of them and adding the one item to the dupes 
let rec powerset = 
   function
   | [] -> [[]]
   | (x::xs) -> 
      let xss = powerset xs 
      List.map (fun xs' -> x::xs') xss @ xss