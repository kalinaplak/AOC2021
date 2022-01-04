#load "../fileReader.fsx"

open AOC.AOCFileReader

let inputNumArr =
    readFileAsNumbers "./FS/day1/input.txt"

let getIncreasedValuesLength =
    fun (s: seq<int>) ->
        s
        |> Seq.pairwise
        |> Seq.filter (fun (x, y) -> x < y)
        |> Seq.length

let pairsL = getIncreasedValuesLength inputNumArr

let triplesL =
    inputNumArr
    |> Seq.zip3 (inputNumArr |> Seq.skip 1) (inputNumArr |> Seq.skip 2)
    |> Seq.map (fun (x, y, z) -> x + y + z)
    |> getIncreasedValuesLength

printfn "%A" pairsL
printfn "%A" triplesL
