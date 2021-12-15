module _2021_1

// Task 1: Count how many times we have a number higher than the previous number
// Task 2: Count how many sums of moving 3-wise pairs are higher than previous (same number in 3 groups)

let private threewiseMoving (source: seq<int>) =
    seq { use ie = source.GetEnumerator()
          let mutable first = 0
          let mutable second = 0

          if (ie.MoveNext()) then
            first <- ie.Current

          if (ie.MoveNext()) then
            second <- ie.Current

          while ie.MoveNext() do
              yield (first, second, ie.Current)
              first <- second
              second <- ie.Current
    }

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let part1 = 
        input 
        |> Seq.map (fun i -> i |> int)
        |> Seq.pairwise 
        |> Seq.filter (fun (first, second) -> first < second) 
        |> Seq.length

    let part2 = 
        input 
        |> Seq.map (fun i -> i |> int)
        |> threewiseMoving
        |> Seq.map (fun (first, second, third) -> first + second + third)
        |> Seq.pairwise
        |> Seq.filter (fun (first, second) -> first < second)
        |> Seq.length

    part1.ToString(), part2.ToString()