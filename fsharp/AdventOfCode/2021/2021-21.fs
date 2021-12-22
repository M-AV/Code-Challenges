module _2021_21

// Task 1: Play simple game with deterministic die
// Task 2: Play with quantum universe splitting die

let charToInt (c:char) = (int c) - (int '0')
let parseInput (input : string seq) = 
    input
    |> Seq.map (fun x -> x[x.Length - 1])
    |> Seq.indexed
    |> Seq.map (fun (x,y) -> charToInt y)
    |> Array.ofSeq

let calcPos currentPos moves = 
    (currentPos + moves - 1) % 10 + 1

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
    

let allRolls = 
    [1;2;3] 
    |> List.allPairs [1;2;3] 
    |> List.allPairs [1;2;3] 
    |> List.map (fun (fst, (snd, trd)) -> fst + snd + trd)
    |> List.countBy id


// I initially misunderstood the problem and thought that the whole multiverse split into 3 
// every time I rolled a die. So I multiplied everything by 3 for every dieroll and my numbers
// exploded.. Took me longer than I'd like to admit to realize my misunderstanding.
// 
// The idea here is fairly simple and based on the fact that there are not that many unique states
// the game can be in. So we're simulating the game, but merging all universes that have the same
// state. 
//
// An alternative implementation I have seen other people use would be a dynamic programming 
// approach, where we recursively calculate and save the result of a given state and in the end
// have a map of all states to their results.
let playGameWithQuantumDice (pl1, pl2) =
    let targetScore = 21
    let mutable stateTracker = Map.empty.Add ((pl1, pl2, 0, 0), 1L)
    let mutable pl1Wins = 0L
    let mutable pl2Wins = 0L

    let updateNewStates key (currentState:int64) pos player (rollMultiplier:int) newStates =
        let (pl1Pos, pl2Pos, pl1Points, pl2Points) = key
        let newKey = 
            if player = 1 then
                (pos, pl2Pos, pl1Points + pos, pl2Points)
            else
                (pl1Pos, pos, pl1Points, pl2Points + pos)
        newStates |> Map.change newKey (fun x -> 
            match x with
            | Some v -> Some(currentState * (int64 rollMultiplier) + v)
            | None -> Some(currentState * (int64 rollMultiplier)))

    let countAndRemoveWonGames (newStates:Map<int*int*int*int, int64>) =
        let mutable newCleanedStates = newStates
        for kv in newCleanedStates do
            let (pl1Pos, pl2Pos, pl1Point, pl2Point) = kv.Key

            if (pl1Point >= targetScore) then
                pl1Wins <- pl1Wins + kv.Value
                newCleanedStates <- newCleanedStates |> Map.remove kv.Key
            else if pl2Point >= targetScore then
                pl2Wins <- pl2Wins + kv.Value
                newCleanedStates <- newCleanedStates |> Map.remove kv.Key  
        newCleanedStates

    while not stateTracker.IsEmpty do
        let mutable newStates = Map.empty
        for curr in stateTracker do
            let (pl1Pos, pl2Pos, pl1Points, pl2Points) = curr.Key

            for (roll, count) in allRolls do
                let pos = calcPos pl1Pos roll
                newStates <- updateNewStates curr.Key curr.Value pos 1 count newStates

        stateTracker <- countAndRemoveWonGames newStates
        newStates <- Map.empty
        for curr in stateTracker do   
            let (pl1Pos, pl2Pos, pl1Points, pl2Points) = curr.Key

            for (roll, count) in allRolls do
                let pos2 = calcPos pl2Pos roll
                newStates <- updateNewStates curr.Key curr.Value pos2 2 count newStates

        stateTracker <- countAndRemoveWonGames newStates

    printfn "Player 1 wins in %A universes" pl1Wins
    printfn "Player 2 wins in %A universes" pl2Wins

    max pl1Wins pl2Wins

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = playGameWithDeterministicDice (parsed[0],parsed[1])

    let part2 = playGameWithQuantumDice (parsed[0],parsed[1])

    part1.ToString(), part2.ToString()