module _2021_25

// Task 1: Find first iteration where no cucumbers move
// Task 2: There is no part 2

let parseInput (input: string seq) =
    let arr = 
        input
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])

let iterate (input:char[,]) =
    let xLength = input.GetLength(0)
    let yLength = input.GetLength(1)
    let moveRight() =
        let mutable didAnyMove = false
        for y in 0 .. yLength - 1  do
            for x in xLength - 1 .. -1 .. 0 do
                if x = xLength - 1 then
                    if input[0, y] = '.' && input[x,y] = '>' then
                        input[x,y] <- '#' // We use '#' to check if the spot is blocked for the next one
                        input[0,y] <- '¤' // We use '¤' to avoid moving the item twice when it wraps around
                        didAnyMove <- true
                else if x = 0 then
                    if input[x,y] = '>' && input[x+1,y] = '.' then
                        input[0,y] <- '.'  
                        input[x+1,y] <- '>'
                        didAnyMove <- true
                    else if input[x,y] = '¤' then
                        input[x,y] <- '>'
                else if input[x,y] = '>' && input[x+1, y] = '.' then
                    input[x,y] <- '#'
                    input[x+1,y] <- '>'
                    didAnyMove <- true
                if x < xLength - 1 && input[x + 1, y] = '#' then
                   input[x + 1, y] <- '.' 
        didAnyMove
    let moveDown() =
        let mutable didAnyMove = false
        for x in 0 .. xLength - 1  do
            for y in yLength - 1 .. -1 .. 0 do
                if y = yLength - 1 then
                    if input[x, 0] = '.' && input[x,y] = 'v' then
                        input[x,y] <- '#'
                        input[x,0] <- '¤'
                        didAnyMove <- true
                else if y = 0 then
                    if input[x,y] = 'v' && input[x,y+1] = '.' then
                        input[x,0] <- '.'
                        input[x,y+1] <- 'v'
                        didAnyMove <- true
                    else if input[x,y] = '¤' then
                        input[x,y] <- 'v'
                else if input[x,y] = 'v' && input[x, y+1] = '.' then
                    input[x,y] <- '#'
                    input[x,y+1] <- 'v'
                    didAnyMove <- true
                if y < yLength - 1 && input[x, y + 1] = '#' then
                    input[x, y + 1] <- '.'
        didAnyMove
        
    let movedRight = moveRight()
    let movedDown = moveDown()
    movedRight || movedDown

let print (input:char[,]) =
    for y in 0 .. input.GetLength(1) - 1 do
        for x in 0 .. input.GetLength(0) - 1 do
            printf "%c" input[x,y]
        printfn ""
    printfn ""

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input
    let mutable iterationCount = 1
    while iterate parsed do
        iterationCount <- iterationCount + 1

    let part1 = iterationCount

    let part2 = "Click the button on the website and save christmas!"

    part1.ToString(), part2.ToString()