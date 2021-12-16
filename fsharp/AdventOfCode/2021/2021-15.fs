module _2021_15

open Xunit
open System.Collections.Generic
open System

// Task 1: Find path lowest sum path through grid with only horizontal and vertical moves
// Task 2: Scale the grid x5 and do it again

let parseInput (input: string seq) =
    let temp = 
        input 
        |> Seq.map (fun x -> x |> Seq.map (fun y -> (int64 y) - (int64 '0')))
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    Array2D.init temp[0].Length temp.Length (fun i j -> temp[i][j])
    
// Due to the example not containing paths that go up or to the left I missed that was possible.
// So this solution works, assuming we can only go down and right. That's not good enough for part 2
// of my input, but it is good enough for the example which took me some time to realize.
let findLowestRiskPath (grid:int64[,]) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid

    let costs = Array2D.create (height) (width) 0L

    for y in 0 .. height-1 do
        for x in 0 .. height-1 do
            match (x,y) with
            | (0,0) -> costs[x,y] <- 0
            | (0, y) -> costs[0,y] <- costs[0, y-1] + grid[x,y]
            | (x, 0) -> costs[x,0] <- costs[x-1, 0] + grid[x,y]
            | (x, y) -> costs[x,y] <-(min costs[x-1, y] costs[x, y-1]) + grid[x,y];

    printfn "%A" costs
    costs[width-1,height-1]

// Here we treat the array as a graph and perform a shortest path analysis
let findLowestRiskPath_ShortestPathAlgo (grid:int64[,]) =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid

    let costs = Array2D.create (height) (width) Int64.MaxValue
    let queue = PriorityQueue<int*int, int64>(); 

    costs[0,0] <- 0
    costs[0,1] <- grid[0,1]
    costs[1,0] <- grid[1,0]
    queue.Enqueue((0,1), grid[0,1])
    queue.Enqueue((1,0), grid[1,0])

    // I don't remember if we have to enqueue every time we change a value,
    // but it shouldn't matter too much for these tasks.
    while queue.Count > 0 do
        let (x,y) = queue.Dequeue()
        if x > 0 then
            let costFromCurrent = costs[x,y] + grid[x-1, y]
            if costs[x-1,y] > costFromCurrent then
                costs[x-1, y] <- costFromCurrent
                queue.Enqueue ((x-1, y), costFromCurrent)
        if x < width - 1 then
            let costFromCurrent = costs[x,y] + grid[x+1, y]
            if costs[x+1, y] > costFromCurrent then
                costs[x+1, y] <- costFromCurrent
                queue.Enqueue ((x+1, y), costFromCurrent)
        if y < height - 1 then
            let costFromCurrent = costs[x, y] + grid[x, y + 1]
            if costs[x, y + 1] > costFromCurrent then
                costs[x, y + 1] <- costFromCurrent
                queue.Enqueue ((x, y + 1), costFromCurrent) 
        if y > 0 then
            let costFromCurrent = costs[x, y] + grid[x, y - 1]
            if costs[x, y-1] > costFromCurrent then
                costs[x, y - 1] <- costFromCurrent
                queue.Enqueue ((x, y - 1), costFromCurrent)

    //printfn "%A" costs
    //printfn "%A" costs[ width - 1, height - 1]

    costs[ width - 1, height - 1]

let scaleGrid (grid:int64[,]) size =
    let height = Array2D.length1 grid
    let width = Array2D.length2 grid
    let newHeight = height * size
    let newWidth = width * size

    let newGrid = Array2D.create newWidth newHeight  0L
    
    let wrapValue value = if value > 9L then 1L else value

    for y in 0 .. newHeight - 1 do
        for x in 0 .. newWidth - 1 do
            if x < width && y < height then
                newGrid[x,y] <- grid[x,y]
            else if x < width then
                newGrid[x,y] <- newGrid[x,y - height] + 1L |> wrapValue
            else if y < height then
                newGrid[x,y] <-  newGrid[x - width,y] + 1L |> wrapValue
            else
                newGrid[x,y] <- newGrid[x - width,y] + 1L |> wrapValue

    //printfn "Scaled: "
    //printfn "%A" newGrid

    newGrid

let execute (input : string seq) =
    printfn "Input count: %i" (Seq.length input)

    let parsed = parseInput input

    let part1 = findLowestRiskPath_ShortestPathAlgo parsed    
    let part2 = findLowestRiskPath_ShortestPathAlgo (scaleGrid parsed 5)

    part1.ToString(), part2.ToString()


[<Fact>]
let ``Example``() =
    let input =
        [
        "1163751742";
        "1381373672";
        "2136511328";
        "3694931569";
        "7463417111";
        "1319128137";
        "1359912421";
        "3125421639";
        "1293138521";
        "2311944581";
        ]
    let (part1, part2) = execute input
    Assert.Equal("40", part1)
    Assert.Equal("315", part2)

[<Fact>]
let ``CustomExample - Here we have to go up``() =
    let input =
        [
            "19111";
            "11191";
            "99991";
            "99991";
            "99991";
        ];
    let (part1, part2) = execute input
    Assert.Equal("10", part1)

[<Fact>]
let ``ScaleGrid Example``() =
    let input =
        [
        "1163751742";
        "1381373672";
        "2136511328";
        "3694931569";
        "7463417111";
        "1319128137";
        "1359912421";
        "3125421639";
        "1293138521";
        "2311944581";
        ]
    let expectedResult = 
        [
            "11637517422274862853338597396444961841755517295286";
            "13813736722492484783351359589446246169155735727126";
            "21365113283247622439435873354154698446526571955763";
            "36949315694715142671582625378269373648937148475914";
            "74634171118574528222968563933317967414442817852555";
            "13191281372421239248353234135946434524615754563572";
            "13599124212461123532357223464346833457545794456865";
            "31254216394236532741534764385264587549637569865174";
            "12931385212314249632342535174345364628545647573965";
            "23119445813422155692453326671356443778246755488935";
            "22748628533385973964449618417555172952866628316397";
            "24924847833513595894462461691557357271266846838237";
            "32476224394358733541546984465265719557637682166874";
            "47151426715826253782693736489371484759148259586125";
            "85745282229685639333179674144428178525553928963666";
            "24212392483532341359464345246157545635726865674683";
            "24611235323572234643468334575457944568656815567976";
            "42365327415347643852645875496375698651748671976285";
            "23142496323425351743453646285456475739656758684176";
            "34221556924533266713564437782467554889357866599146";
            "33859739644496184175551729528666283163977739427418";
            "35135958944624616915573572712668468382377957949348";
            "43587335415469844652657195576376821668748793277985";
            "58262537826937364893714847591482595861259361697236";
            "96856393331796741444281785255539289636664139174777";
            "35323413594643452461575456357268656746837976785794";
            "35722346434683345754579445686568155679767926678187";
            "53476438526458754963756986517486719762859782187396";
            "34253517434536462854564757396567586841767869795287";
            "45332667135644377824675548893578665991468977611257";
            "44961841755517295286662831639777394274188841538529";
            "46246169155735727126684683823779579493488168151459";
            "54698446526571955763768216687487932779859814388196";
            "69373648937148475914825958612593616972361472718347";
            "17967414442817852555392896366641391747775241285888";
            "46434524615754563572686567468379767857948187896815";
            "46833457545794456865681556797679266781878137789298";
            "64587549637569865174867197628597821873961893298417";
            "45364628545647573965675868417678697952878971816398";
            "56443778246755488935786659914689776112579188722368";
            "55172952866628316397773942741888415385299952649631";
            "57357271266846838237795794934881681514599279262561";
            "65719557637682166874879327798598143881961925499217";
            "71484759148259586125936169723614727183472583829458";
            "28178525553928963666413917477752412858886352396999";
            "57545635726865674683797678579481878968159298917926";
            "57944568656815567976792667818781377892989248891319";
            "75698651748671976285978218739618932984172914319528";
            "56475739656758684176786979528789718163989182927419";
            "67554889357866599146897761125791887223681299833479";
        ]
    let parsed = parseInput input
    let parsedResult = parseInput expectedResult
    let grid = scaleGrid parsed 5

    Assert.Equal(50, Array2D.length1 grid)
    Assert.Equal(50, Array2D.length2 grid)

    for y in 0 .. Array2D.length2 parsedResult - 1 do
        for x in 0 .. Array2D.length1 parsedResult - 1 do
            Assert.Equal(parsedResult[x,y], grid[x,y])