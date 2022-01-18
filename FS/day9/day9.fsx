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
