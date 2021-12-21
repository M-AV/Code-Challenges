module _2021_21

// Task 1: 
// Task 2: 
let charToInt (c:char) = (int c) - (int '0')
let parseInput (input : string seq) = 
    input
    |> Seq.map (fun x -> x[x.Length - 1])
    |> Seq.indexed
    |> Seq.map (fun (x,y) -> charToInt y)
    |> Array.ofSeq

let playGameWithDeterministicDice initialState = 
    let mutable nextDieValue = 1
    let mutable dieCount = 0
    let rollDie() = 
        dieCount <- dieCount + 1
        let res = nextDieValue
        if nextDieValue = 100 then
            nextDieValue <- 1
        else
            nextDieValue <- nextDieValue + 1
        res
    let calcPos currentPos moves = 
        let mutable newPos = currentPos + moves
        while newPos > 10 do
            newPos <- newPos - 10
        newPos
    let mutable playerStatus = Map.empty.Add(1, (0, fst initialState)).Add(2, (0, snd initialState))
    let mutable winner = 0

    let roll player =
        let dieResult = rollDie() + rollDie() + rollDie()
        let (score, pos) = playerStatus[player]
        let newPosition = calcPos pos dieResult
        playerStatus <- playerStatus |> Map.change player (fun x -> 
            match x with 
            | Some (_, _) -> Some(score + newPosition, newPosition))
        if score + newPosition >= 1000 then
            winner <- player

        //printfn "Player %i rolls %i - moves to %i - score %i" player dieResult newPosition (score + newPosition)

        score + newPosition
    
    while winner = 0 do
        let score = roll 1
        if score < 1000 then
            roll 2 |> ignore
    
    let status = 
        if winner = 1 then
            playerStatus[2]
        else 
            playerStatus[1]
    dieCount * (fst status)
    
    //let calcPoints 

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = playGameWithDeterministicDice (parsed[0],parsed[1])

    let part2 = "N/A"

    part1.ToString(), part2.ToString()