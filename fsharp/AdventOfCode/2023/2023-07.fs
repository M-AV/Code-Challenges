module _2023_07

open InputProvider
open Calculations
open Parsing
open System
open Xunit

let parseInput (input : string seq) = 
    input
    |> Seq.map (fun x -> x.Split ' ')
    |> Seq.map (fun x -> x[0] |> Array.ofSeq, int64 x[1])
    |> List.ofSeq

let getHandType hand =
    let distinct = hand |> Array.distinct 
    let groups = hand |> Array.groupBy id |> Array.sortByDescending (fun (_,y) -> y.Length)

    if distinct.Length = 1 then 7 // Five of a kind
    else if distinct.Length = 2 then 
        if (snd groups[0]).Length = 4 then 6 // Four of a kind
        else 5 // Full House
    else if distinct.Length = 3 then 
        if (snd groups[0]).Length = 3 then 4 // Three of a kind
        else 3 // Two pairs    
    else if distinct.Length = 4 then 2 // One pair
    else if distinct.Length = 5 then 1 // High card
    else 1 // Shouldn't happen

let getCardRank card withJoker =
    let res = 
        match card with
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | 'A' -> 14

    if withJoker && res = 11 then
        1
    else
        res

let compareHands withJoker ghtFunc (first:char array * int64 * int) second =
    let compareEqualHands handOne handTwo =
        handOne 
        |> Array.zip handTwo
        |> Array.fold (fun acc curr -> 
            if acc <> 0 then
                acc
            else
                let s = getCardRank (snd curr) withJoker - getCardRank (fst curr) withJoker
                s
            ) 0

    let firstRank = ghtFunc first
    let secondRank = ghtFunc second

    let (f,_,_) = first
    let (s,_,_) = second

    if firstRank = secondRank then
        compareEqualHands f s
    else
        firstRank - secondRank


let part1 (input:(char array * int64) list) =
    let cmp = compareHands false trd

    input
    |> Seq.map (fun (x,y) -> (x, y, getHandType x))
    |> Seq.sortWith cmp
    |> Seq.indexed
    |> Seq.map (fun (i, (_, v, _)) -> ((int64 i)+1L) * (int64 v))
    |> Seq.sum

let part2 (input:(char array * int64) list) =

    let replaceJokers hand =
        let JCount = hand |> Array.filter ((=) 'J') |> Array.length
        // I didn't know if the complexity would explode due to the number of combinations, 
        // so here i special cased 'JJJJ' and any hand with 4 'J's, as they would always 
        // end up with 5 of a kind
        if JCount = 0 || JCount = 5 then
            [| hand |]
        else if JCount = 4 then
            let nonJChar = hand |> Array.filter ((<>) 'J') |> Array.head
            [| [|nonJChar; nonJChar; nonJChar; nonJChar; nonJChar|] |]
        else
            let values = [|'2';'3';'4';'5';'6';'7';'8';'9';'T';'Q';'K';'A'|]

            let possibleValues x = 
                if x = 'J' then
                    values
                else [|x|]

            [| for fst in possibleValues hand[0] do
                for snd in possibleValues hand[1] do
                    for trd in possibleValues hand[2] do
                        for fourth in possibleValues hand[3] do
                            for fifth in possibleValues hand[4] do
                                [| fst; snd; trd; fourth; fifth |]
            |]

    let findMaxRank hand = 
        let h = replaceJokers hand

        let res = 
            h 
            |> Array.map getHandType
            |> Array.max
        res

    let cmp = compareHands true trd

    let s = 
        input
        |> Seq.map (fun (x,y) -> (x, y, findMaxRank x))
        |> Seq.sortWith cmp
        |> Seq.indexed
        |> Seq.map (fun (i, (_, v, _)) -> ((int64 i)+1L) * (int64 v))
        |> Seq.sum

    s

let execute (input : string seq) =
    let parsed = parseInput input

    let part1 = part1 parsed

    let part2 = part2 parsed

    part1.ToString(), part2.ToString()

[<Fact>]
let ``Test``() =
    let (part1, part2) = execute (getPuzzleInput "2023" "7" |> Async.RunSynchronously)
    Assert.Equal("253866470", part1)
    Assert.Equal("254494947", part2)