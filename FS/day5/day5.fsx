#load "../fileReader.fsx"

open AOC.AOCFileReader

let fileLineToPointPairs (l:string) =
     let pairArr = l.Split(" -> ") |> Array.map ( fun p -> 
        let pointArr = p.Split(",") |> Array.map (fun str -> str |> int)
        (pointArr.[0], pointArr.[1])
     )
     (pairArr.[0], pairArr.[1])

let filterHorizontalOrVerticalLines pairs =
    pairs |> Array.filter (fun p ->
        let (x,y) = fst p
        let (x2,y2) = snd p
        x = x2 || y = y2
    )
    
let rec pointPairToPoints ((x1,y1), (x2,y2)) cx cy (points:List<(int * int)>) =
    if cx = x2 && cy = y2 then points 
    else 
        let nx = if cx < x2 then (cx + 1) else if cx <> x2 then (cx - 1) else cx
        let ny = if cy < y2 then (cy + 1) else if cy <> y2 then (cy - 1) else cy
        let newPoints = 
            if cx = x1 && cy = y1 
            then List.append points [(x1,y1); (nx,ny)] 
            else List.append points [(nx,ny)]
        pointPairToPoints ((x1,y1), (x2,y2)) nx ny newPoints

let getPointsList points =
    List.concat (points |> Array.map ( fun p -> 
        pointPairToPoints p (fst (fst p)) (snd (fst p)) List.empty
    ))

let groupPoints points =
    points |> List.groupBy id |> List.map (fun (c, cs) -> c, List.length cs)

let filterGroupsByLength groups number =
    (groups |> List.filter (fun p -> snd p > number)).Length

//1.
let pointPairs = readFileAsStringLines "./FS/day5/input.txt" |> Array.map fileLineToPointPairs
let pointsGrouped = filterHorizontalOrVerticalLines pointPairs |> getPointsList |> groupPoints
let result = filterGroupsByLength pointsGrouped 1 
printfn "%A" result

//2.
let diagonalPoints = pointPairs |> getPointsList |> groupPoints
let result2 = filterGroupsByLength diagonalPoints 1
printfn "%A" result2