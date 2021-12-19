module _2021_18

open System
open Xunit

// Task 1: Help the fish with additions
// Task 2: Find combination of numbers that gives us highest magnitude

// Initially I used a int*int as the key into the map, but it didn't work and somewhat complicated 
// so now we just use Guid
type Pair = 
    | Literal of int
    | Pair of Guid * Guid
type Tree = Guid * Map<Guid, Pair>

let charToInt (c:char) = (int c) - (int '0')

let rec print (tree:Tree) : string =
    let map = snd tree
    let mutable str = "";
    match map[(fst tree)] with 
    | Literal v -> 
        str <- (str + v.ToString())
    | Pair (lft, rgt) ->
        str <- str + "["
        str <- str + (print (lft, map))
        str <- str + ","
        str <- str + (print (rgt, map))
        str <- str + "]"
    str


// We represent the each tree as a Map<int*int, Pair> where the key is the line number and a generated integer.
// For the generated integer, the root should have the highest one in the tree
// That way the key is unique across all trees.
// We then return the map along with the key for the root Pair.
let parseInput (input:string seq) : Tree seq =
    let rec parsePair (map:Map<Guid, Pair>) line : Guid * int * Map<Guid, Pair> =
        match line with
        | '['::tail ->
            let (leftIdx, count, map) = parsePair map tail
            let (rightIdx, count2, map) = parsePair map (tail |> List.skip (count + 1))
            let usedChars = count + count2 + 3
            let myIdx = Guid.NewGuid()
            let updatedMap = map |> Map.add myIdx (Pair(leftIdx, rightIdx))

            myIdx, usedChars, updatedMap
        | x::tail when Char.IsDigit x -> 
            let leaf = Literal(charToInt x)
            let idx = Guid.NewGuid()
            let updatedMap = map |> Map.add idx leaf
            (idx, 1, updatedMap)

    let x = 
        input 
        |> Seq.map List.ofSeq
        |> Seq.map (fun x -> parsePair Map.empty x)
        |> Seq.map (fun (rootIdx, _, map) -> (rootIdx, map))
    x

let getLiteralValue key (map:Map<Guid, Pair>) = match map[key] with | Literal x -> x

let fishAdd (left:Tree) (right:Tree) =
    let mergedMaps = Map.fold (fun acc key value -> Map.add key value acc) (snd left) (snd right)
    let newRootKey = Guid.NewGuid()
    let updatedMap = Map.add newRootKey (Pair(fst left, fst right)) mergedMaps
    //printfn "A: %A" (print (newRootKey, updatedMap))
    (newRootKey, updatedMap)


let explode (tree:Tree) = 
    let mutable map = snd tree
    

    // The idea here is to traverse the tree and always keep track of the index into the left value.
    // That way when we find the value we need to explode we can replace it with 0 AND fix the left value
    // immediately. Then we continue the traversal until we hit the value on the right and replace that
    // 
    // It started out farly simple and then it... turned into this. ¯\_(ツ)_/¯
    // I'm sure it could be cleaned up.. But whatevs
    let rec traverse key leftNeighbor depth (addToFirstLiteral:int option) replacedLeft replacedRight : (Guid) * bool * bool * int option =
        if replacedLeft && replacedRight then
            (key, true, true, None)
        else
            match (map[key], depth, addToFirstLiteral) with
            | (Literal x, _, Some value) ->
                // Update right
                map <- map |> Map.change key (fun x ->
                    match x with 
                    | Some s -> Some(Literal ((getLiteralValue key map) + value)))
                (key, true, true, None)
            | (Literal _, _, _) -> (key, false, false, None)
            | (Pair (left, right), dp, None) when dp > 3 -> 
                let leftValue = getLiteralValue left map
                let rightValue = getLiteralValue right map

                // Remove literals and save indexes for splitting purposes
                map <- map |> Map.remove left
                map <- map |> Map.remove right
                // Set to 0
                map <- map |> Map.change key (fun x -> 
                    match x with
                    | Some s -> Some(Literal 0))
                // Update left
                map <- map |> Map.change leftNeighbor (fun x -> 
                    match x with
                    | Some s -> Some(Literal ((getLiteralValue leftNeighbor map) + leftValue))
                    | None -> None)

                (key, true, false, Some(rightValue))
            | (Pair (left, right), dp, _) ->
                let (lftNeighbor, lhLeft, lhRight, addToRight) = traverse left leftNeighbor (depth + 1) addToFirstLiteral replacedLeft replacedRight
                let (lftNeighbor, rhLeft, rhRight, addToRight) = traverse right lftNeighbor (depth + 1) addToRight (replacedLeft || lhLeft) (replacedRight || lhRight)
                (lftNeighbor, lhLeft || rhLeft || replacedLeft, lhRight || rhRight || replacedRight, addToRight)

    let (_, didXplode, _, _) = traverse (fst tree) Guid.Empty 0 None false false
    (didXplode, (fst tree, map))

let splitTree (tree:Tree) =
    let mutable map = snd tree
    let rec traverse key didSplit =
        match (didSplit, map[key]) with
        | (true, _) -> true
        | (_, Literal x) when x > 9 -> 
            let newLeftVal = Literal (x / 2)
            let newRightVal = Literal ((x - 1) / 2 + 1)

            let leftKey = Guid.NewGuid()
            let rightKey = Guid.NewGuid()

            map <- map |> Map.add leftKey newLeftVal
            map <- map |> Map.add rightKey newRightVal
            map <- map |> Map.change key (fun x -> 
                match x with
                | Some s -> Some(Pair(leftKey, rightKey)))
            true
        | (_, Literal x) -> didSplit
        | (_, Pair (left, right)) ->
            let didSplit = traverse left didSplit
            traverse right didSplit
    let (didSplit) = traverse (fst tree) false
    (didSplit, (fst tree, map))

let reduceFishNumber (tree:Tree) =
    let mutable didExplode = false
    let mutable didSplit = false
    let mutable tree = tree

    // Exploding will remove 2 literals from the tree and split will add 2.
    // I'm hoping I can always use the deleted ones to insert new ones. If not, I will have to find a new 
    // scheme for indexing
    let (xplod, t) = explode tree
    didExplode <- xplod
    tree <- t
    //printfn "R: %A" (print tree)
    while didExplode || didSplit do
        let (xplod, t) = explode tree
        didExplode <- xplod
        tree <- t
        if not didExplode then
            let (spl, t) = splitTree tree 
            didSplit <- spl
            tree <- t
        //printfn "R: %A" (print tree)
    
    tree

        
let calcMagnitude (tree:Tree) =
    let map = snd tree
    let rec traverse idx =
        match (map[idx]) with
        | Literal x -> x
        | Pair (lft, rgt) -> 
            3 * (traverse lft) + 2 * (traverse rgt)
    traverse (fst tree)

let addAndReduce (tree:Tree seq) =
    let head = tree |> Seq.head
    let added = 
        tree
        |> Seq.skip 1
        |> Seq.fold (fun agg cur ->  fishAdd agg cur |> reduceFishNumber) head
    added

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let added = addAndReduce parsed
    //let str = print added

    let part1 = calcMagnitude added

    // We're just bruteforcing the solution. It's quite slow
    let part2 = 
        Seq.allPairs parsed parsed
        |> Seq.map (fun (fst, snd) -> 
            if fst = snd 
                then -1
            else
                let fstRes = fishAdd fst snd |> reduceFishNumber |> calcMagnitude
                let sndRes = fishAdd snd fst |> reduceFishNumber |> calcMagnitude
                max fstRes sndRes
            )
        |> Seq.max

    part1.ToString(), part2.ToString()



[<Fact>]
let ``Adding 1``() =
    let input = [ "[1,2]"; "[1,2]" ]
    let parsed = parseInput input |> Array.ofSeq

    let added = fishAdd parsed[0] parsed[1] 

    added

[<Fact>]
let ``AddingAndReducing - Example 1``() =
    let input = [
        "[1,1]";
        "[2,2]";
        "[3,3]";
        "[4,4]"
        ]
    let parsed = parseInput input
    let res = addAndReduce parsed
    let str = print res
    Assert.Equal ("[[[[1,1],[2,2]],[3,3]],[4,4]]", str)

[<Fact>]
let ``AddingAndReducing - Example 2``() =
    let input = [
        "[1,1]";
        "[2,2]";
        "[3,3]";
        "[4,4]";
        "[5,5]"
        ]
    let parsed = parseInput input
    let res = addAndReduce parsed
    let str = print res
    Assert.Equal ("[[[[3,0],[5,3]],[4,4]],[5,5]]", str)

[<Fact>]
let ``Parsing Example 1``() =
    let input = [ "[1,2]" ]
    let (rootIdx, map) = parseInput input |> Seq.head

    let root = map[rootIdx]

    match root with
    | Pair (fst, snd) -> 
        match map[fst] with | Literal x -> Assert.Equal(1, x)
        match map[snd] with | Literal x -> Assert.Equal(2, x)

[<Fact>]
let ``Parsing Example 2``() =
    let input = [ "[[1,2],3]" ]
    let (rootIdx, map) = parseInput input |> Seq.head
        
    match map[rootIdx] with
    | Pair (fst, snd) -> 
        match map[fst] with | Pair (fst, snd) -> Assert.Equal(1, 1)
        match map[snd] with | Literal x -> Assert.Equal(3, x)

[<Fact>]
let ``Parsing Examples``() =
    let input = [ 
        "[1,2]";
        "[[1,2],3]";
        "[9,[8,7]]";
        "[[1,9],[8,5]]";
        "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]";
        "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]";
        "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"; ]
    let parsed = parseInput input |> List.ofSeq |> List.head
    2

[<Theory>]
[<InlineData("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")>]
[<InlineData("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")>]
[<InlineData("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")>]

[<InlineData("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")>]

[<InlineData("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
[<InlineData("[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]", "[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]" )>] // Step 1
[<InlineData("[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]", "[[[[3,0],[5,3]],[4,4]],[5,5]]" )>] // Step 2
let ``Explode Examples`` input expectedRes =
    let input = [ input ]
    let parsed = parseInput input |> List.ofSeq |> List.head
    let (didExplode, res) = explode parsed
    Assert.True(didExplode)
    Assert.Equal(expectedRes, print res)

[<Theory>]
[<InlineData("[9,1]", 29)>]
[<InlineData("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
[<InlineData("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)>]
[<InlineData("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
let ``Magnitude`` input expectedRes =
    let input = [ input ]
    let parsed = parseInput input
    let magni = calcMagnitude (parsed |> Seq.head)
    Assert.Equal (expectedRes, magni)

[<Fact>]
let ``part 1 - Example``() =
    let input = [ 
        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
        "[[[5,[2,8]],4],[5,[[9,9],0]]]";
        "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
        "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
        "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
        "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
        "[[[[5,4],[7,7]],8],[[8,3],8]]";
        "[[9,3],[[9,9],[6,[4,9]]]]";
        "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
        "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"; ]
    let (part1, part2) = execute input

    Assert.Equal("4140", part1)