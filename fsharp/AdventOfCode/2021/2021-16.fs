module _2021_16

open Xunit
open System

// Task 1: Sum versions of all packets
// Task 2: Evaluate packets and find result

type Version = int
type TypeId = int
type Packet =
    | Literal of Version * TypeId * int64 
    | Operator of Version * TypeId * (Packet list)

let parseInput input =
    input
    |> Seq.head
    |> List.ofSeq

let hexMap = 
    Map [ 
       ('0', [0;0;0;0]);
       ('1', [0;0;0;1]);
       ('2', [0;0;1;0]);
       ('3', [0;0;1;1]);
       ('4', [0;1;0;0]);
       ('5', [0;1;0;1]);
       ('6', [0;1;1;0]);
       ('7', [0;1;1;1]);
       ('8', [1;0;0;0]);
       ('9', [1;0;0;1]);
       ('A', [1;0;1;0]);
       ('B', [1;0;1;1]);
       ('C', [1;1;0;0]);
       ('D', [1;1;0;1]);
       ('E', [1;1;1;0]);
       ('F', [1;1;1;1])]
let parseLiteral code =
    match code with
    | [0;0;0;0] -> '0'
    | [0;0;0;1] -> '1'
    | [0;0;1;0] -> '2'
    | [0;0;1;1] -> '3'
    | [0;1;0;0] -> '4'
    | [0;1;0;1] -> '5'
    | [0;1;1;0] -> '6'
    | [0;1;1;1] -> '7'
    | [1;0;0;0] -> '8'
    | [1;0;0;1] -> '9'
    | [1;0;1;0] -> 'A'
    | [1;0;1;1] -> 'B'
    | [1;1;0;0] -> 'C'
    | [1;1;0;1] -> 'D'
    | [1;1;1;0] -> 'E'
    | [1;1;1;1] -> 'F'
    

let hexToBits code =
    code 
    |> List.map (fun x -> hexMap[x])
    |> List.collect id

let nextPacketTypeOrVersion code =
    match code with
    | 0::0::0::tail -> (0, tail)
    | 0::0::1::tail -> (1, tail)
    | 0::1::0::tail -> (2, tail)
    | 0::1::1::tail -> (3, tail)
    | 1::0::0::tail -> (4, tail)
    | 1::0::1::tail -> (5, tail)
    | 1::1::0::tail -> (6, tail)
    | 1::1::1::tail -> (7, tail)
let convertBitsToInt (bits:int list) =
    let mutable res = 0
    for i in bits do
        res <- res <<< 1
        res <- res ||| i
    res
let convertBitsToInt64 (bits:int list) =
    let mutable res = 0L
    for i in bits do
        res <- res <<< 1
        res <- res ||| i
    res

let rec parsePackets code : Packet list * int list =
    let rec parseType4Packet version typeId (code:int list) : Packet * int list =
        let rec findBits result remaining =
            match remaining with
            | 0::tail ->  // Last value
                (result@(tail |> List.take 4), tail |> List.skip 4)
            | 1::tail ->  // Not last value
                let literal = tail |> List.take 4
                findBits (result@literal) (tail |> List.skip 4)
        let (value, rem) = code |> findBits []
        (Literal(version, typeId, convertBitsToInt64 value), rem)
    
    and parseOperatorPacket version typeId (code:int list) : Packet * int list = 
        match code with
        | 0::tail -> // first 15 bits are a number representing the total length in bits of the subpackets
            let totalLength = convertBitsToInt (tail |> List.take 15)
            let (subPackets, rem) = tail |> List.skip 15 |> List.take totalLength |> parsePackets
            if rem.Length > 0 then
                printfn "Err? %A" rem
            let remaining = tail |> List.skip (15 + totalLength)
            (Operator(version, typeId, subPackets), remaining)
        | 1::tail -> // First 11 bits are a number that represents the number of sub packets in this packet
            let numberOfPackets = convertBitsToInt (tail |> List.take 11)
            let (packets, remaining) = parseExactNumberOfPackets numberOfPackets (tail |> List.skip 11)
            (Operator(version, typeId, packets), remaining)         

    and parseNextPacket code : Packet option * int list= 
        if List.length code < 6 then
            (None, code)
        else
            let (version, rem) = nextPacketTypeOrVersion code
            let (typeId, rem) = nextPacketTypeOrVersion rem
            match typeId with
            | 4 ->  parseType4Packet version typeId rem |> (fun (packet, list) -> (Some packet, list))
            | _ when List.length code > 10 -> parseOperatorPacket version typeId rem |> (fun (packet, list) -> (Some packet, list))
            | _ -> (None, code)

    and parseExactNumberOfPackets number code =
        let mutable result = []
        let mutable rem = code
        for _ in 1 .. number do
            let (packet, r) = parseNextPacket rem
            rem <- r
            result <- packet :: result
        (result |> List.map (fun x -> x.Value) |> List.rev, rem) // I believe all of these should have a value
    and parseAll code (result:Packet list) =
        let (packet, rem) = parseNextPacket code
        match packet with
        | Some x -> parseAll rem (x::result)
        | None -> (result |> List.rev, rem)

    if code.Length < 6 then 
        ([], code) // We're all the way at the end and discard remaining 0 bits
    else 
        parseAll code []
   
let rec sumVersionNumbers packets = 
    let rec sumVersions packet =
        match packet with
        | Literal (version, _, _) -> version
        | Operator (version, _, subs) -> version + (sumVersionNumbers subs)
    let mutable sum = 0
    for packet in packets do
        sum <- sumVersions packet + sum
    sum

let evaluatePackets packets : int64=
    let packet = packets |> List.head // From the task it says "value of the outermost packet", so I assume there is only 1
    let rec evaluate packet =
        match packet with 
        | Literal (version, typ, value) -> int64 value
        | Operator (version, typ, subs) ->
            let results = subs |> List.map evaluate
            match typ with
            | 0 -> results |> Seq.sum
            | 1 -> results |> Seq.fold (fun agg curr -> agg * curr) 1L
            | 2 -> results |> Seq.min
            | 3 -> results |> Seq.max
            | 5 -> if results.Head > results.Tail.Head then 1 else 0L
            | 6 -> if results.Head < results.Tail.Head then 1 else 0L
            | 7 -> if results.Head = results.Tail.Head then 1 else 0L
    evaluate packet

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input |> hexToBits

    let (packets, rem) = parsePackets parsed

    //printfn "%A" packets

    let part1 = sumVersionNumbers packets

    let part2 = evaluatePackets packets

    part1.ToString(), part2.ToString()

let isLiteral packet code =
    match packet with
    | Literal (_, _, x) -> Assert.Equal(code, String.Concat x)
    | _ -> Assert.True(false)

[<Fact>]
let ``HexToBits - Example 1``() =
    let input = [ "D2FE28" ]
    let parsed = parseInput input
    let bits = hexToBits parsed
    let expectedResult = [1;1;0;1;0;0;1;0;1;1;1;1;1;1;1;0;0;0;1;0;1;0;0;0]
    Assert.Equal<int>(expectedResult, bits)

[<Fact>]
let ``Packet Version parsing``() =
    let version  = nextPacketTypeOrVersion [1;1;0;1;0;0;1;0;1;1;1;1;1;1;1;0;0;0;1;0;1;0;0;0]
    Assert.Equal(6, fst version)

[<Fact>]
let ``PacketType parsing``() =
    let version  = nextPacketTypeOrVersion [1;0;0;1;0;1;1;1;1;1;1;1;0;0;0;1;0;1;0;0;0]
    Assert.Equal(4, fst version)
[<Fact>]
let ``Type 4 Parsing - Example``() = 
    let (packets, rem) = parsePackets [1;1;0;1;0;0;1;0;1;1;1;1;1;1;1;0;0;0;1;0;1;0;0;0]
    match packets |> List.head with
    | Literal(_,_, x) -> Assert.Equal("2021", String.Concat x)
    | _ -> Assert.False(true)
    
[<Fact>]
let ``Operator Packet - Example 1``() =
    let (packets, rem) = parsePackets [0;0;1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;1;1;1;1;0;1;0;0;0;1;0;1;0;0;1;0;1;0;0;1;0;0;0;1;0;0;1;0;0;0;0;0;0;0;0;0]
    Assert.Equal(1, packets.Length)

    match packets.Head with
    | Operator (_,_,sub) -> 
        Assert.Equal(2, sub.Length)
        isLiteral sub.Head "10"|> ignore
        isLiteral (sub |> List.skip 1 |> List.head) "20" |> ignore
    | _ -> Assert.False(true)
[<Fact>]
let ``Operator Packet - Example 2``() =
    let (packets, rem) = parsePackets [1;1;1;0;1;1;1;0;0;0;0;0;0;0;0;0;1;1;0;1;0;1;0;0;0;0;0;0;1;1;0;0;1;0;0;0;0;0;1;0;0;0;1;1;0;0;0;0;0;1;1;0;0;0;0;0]
    match packets.Head with
    | Operator (_,_,sub) -> 
        Assert.Equal(3, sub.Length)
        isLiteral sub.Head "1" |> ignore
        isLiteral (sub |> List.skip 1 |> List.head) "2" |> ignore
        isLiteral (sub |> List.skip 2 |> List.head) "3" |> ignore
    | _ -> Assert.False(true)

[<Fact>]
let ``Part 2 - Example 1``() =
    let input = [ "C200B40A82" ]
    let (_, part2) = execute input

    Assert.Equal("3", part2)
[<Fact>]
let ``Part 2 - Example 2``() =
    let input = [ "04005AC33890" ]
    let (_, part2) = execute input

    Assert.Equal("54", part2)
[<Fact>]
let ``Part 2 - Example 3``() =
    let input = [ "880086C3E88112" ]
    let (_, part2) = execute input

    Assert.Equal("7", part2)
[<Fact>]
let ``Part 2 - Example 4``() =
    let input = [ "CE00C43D881120" ]
    let (_, part2) = execute input

    Assert.Equal("9", part2)
[<Fact>]
let ``Part 2 - Example 5``() =
    let input = [ "D8005AC2A8F0" ]
    let (_, part2) = execute input

    Assert.Equal("1", part2)
[<Fact>]
let ``Part 2 - Example 6``() =
    let input = [ "F600BC2D8F" ]
    let (_, part2) = execute input

    Assert.Equal("0", part2)
[<Fact>]
let ``Part 2 - Example 7``() =
    let input = [ "9C005AC2F8F0" ]
    let (_, part2) = execute input

    Assert.Equal("0", part2)
[<Fact>]
let ``Part 2 - Example 8``() =
    let input = [ "9C0141080250320F1802104A08" ]
    let (_, part2) = execute input

    Assert.Equal("1", part2)