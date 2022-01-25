#load "../fileReader.fsx"
open AOC.AOCFileReader

let heightmap =
    readFileAsStringLines "./FS/day9/input.txt"
    |> Array.map (fun line -> Seq.toList line |> Seq.map (fun el -> el |> sprintf "%c" |> int) |> Seq.toArray)

let checkLowestLocation i j (heightmap: int[][]) =
    let maxI = (heightmap.Length - 1)
    let maxJ = (heightmap.[0].Length - 1)
    let value = heightmap.[i].[j]
    let isLowerTop = if i <> 0 then value < heightmap.[i - 1].[j] else true
    let isLowerLeft = if j <> 0 then value < heightmap.[i].[j - 1] else true
    let isLowerBottom = if i < maxI then value < heightmap.[i + 1].[j] else true
    let isLowerRight = if j < maxJ then value < heightmap.[i].[j + 1] else true
    if (isLowerTop && isLowerLeft && isLowerBottom && isLowerRight) then value else -1

let rec findLowestPoints i j (heightmap: int[][]) (lowestPoints: list<int>) =
    if i = heightmap.Length then lowestPoints
    else if j = heightmap.[0].Length then findLowestPoints (i + 1) 0 heightmap lowestPoints
    else 
        let lowestLoc = checkLowestLocation i j heightmap
        if lowestLoc <> -1 then findLowestPoints i (j + 1) heightmap (List.append lowestPoints [lowestLoc])
        else findLowestPoints i (j + 1) heightmap lowestPoints

let getRiskSum (lowest:list<int>) = lowest |> List.map (fun l -> l + 1) |> List.sum

let result = getRiskSum (findLowestPoints 0 0 heightmap [])
printf "%A" result 

// 2.
let rec getBasinMap i j basinRows (heightmap:int[][]) =
    if(i < heightmap.Length && j < heightmap.[0].Length) then
        if(heightmap.[i].[j] < 9 ) 
        then getBasinMap i (j + 1) (basinRows |> Map.add (i,j) heightmap.[i].[j]) heightmap
        else getBasinMap i (j + 1) basinRows heightmap
    else if (i < heightmap.Length) 
        then getBasinMap (i + 1) 0 basinRows heightmap
        else basinRows

let getClosestNeigbours i j (list: List<(int * int) * int>) = 
    list |> List.filter(fun el -> 
        let (eli, elj) = fst el
        let idiff = abs (eli - i)
        let jdiff = abs (elj - j)
        (idiff = 0 && jdiff = 1) || (idiff = 1 && jdiff = 0)
    )

let rec getAllNeighbours (elemsToCheck: List<(int * int) * int>) (allElements: List<(int * int) * int>) (result: List<(int * int) * int>) =
    match elemsToCheck with
    | head::tail -> 
        let (i,j) = fst head
        let neighbours = getClosestNeigbours i j allElements
        if(neighbours.Length <> 0) 
        then
            let checkEl = ((List.append tail neighbours) |> List.distinct) |> List.except result
            getAllNeighbours checkEl allElements (List.append [head] result)
        else getAllNeighbours tail allElements (List.append [head] result)
    | _ -> result

let rec getAllBasins (elemsToCheck: List<(int * int) * int>) (basins: List<List<(int * int) * int>>) =
    match elemsToCheck with
    | head:: tail ->
        let basin = getAllNeighbours [head] tail List.empty
        let rest = tail |> List.except basin
        getAllBasins rest (List.append [basin] basins)
    | _ -> basins

let getLargestBasinsSizeMultiply basinList = 
    let basins = getAllBasins basinList List.empty
    basins |> List.map( fun x -> x.Length) |> List.sortDescending |> List.take 3 |> List.reduce (fun a b -> a * b)

let basinList = getBasinMap 0 0 Map.empty heightmap |> Map.toList
let result2 = getLargestBasinsSizeMultiply basinList
printf "%A" result2
