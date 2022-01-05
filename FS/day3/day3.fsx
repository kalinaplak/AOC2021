#load "../fileReader.fsx"

open AOC.AOCFileReader

let getColumn (array: array<list<char>>) (index: int) =
    array
    |> Array.map (fun bits -> bits.[index])
    |> Array.toList

let rec calcColumn acc list =
    match list with
    | head :: tail ->
        if head = '0' then
            calcColumn ((fst acc + 1), snd acc) tail
        else
            calcColumn (fst acc, (snd acc + 1)) tail
    | [] -> acc

let calcGamma (l: array<list<char>>) =
    (l.[0])
    |> List.mapi (fun i v -> calcColumn (0, 0) (getColumn l i))
    |> List.map (fun v -> if fst v > snd v then "0" else "1")

let reverseBitList list =
    list
    |> List.map (fun v -> if v = "0" then "1" else "0")

let binaryStringToInt input =
    System.Convert.ToInt32(input |> List.fold (+) "", 2)

let getFinalRatio input1 input2 =
    binaryStringToInt input1
    * binaryStringToInt input2

let bits =
    readFileAsStringLines "./FS/day3/input.txt"
    |> Array.map (fun line -> Seq.toList line)

let gamma = calcGamma bits
let epsilon = gamma |> reverseBitList
let powerConsumption = getFinalRatio gamma epsilon
printfn "%A" powerConsumption

//2.

let calcEpsilon bits = calcGamma bits |> reverseBitList

let rec calcRating pos (bits: array<list<char>>) factorFn =
    let newFactor: list<string> = factorFn bits

    let filtered =
        bits
        |> Array.filter (fun row -> (string row.[pos]) = newFactor.[pos])

    match filtered.Length with
    | 0 ->
        if pos < newFactor.Length then
            calcRating (pos + 1) bits factorFn
        else
            []
    | 1 -> filtered.[0] |> List.map (fun b -> b.ToString())
    | _ -> calcRating (pos + 1) filtered factorFn

let oxygen = calcRating 0 bits calcGamma
let co2 = calcRating 0 bits calcEpsilon

let lifeSupportRating = getFinalRatio oxygen co2
printfn "%A" (oxygen |> binaryStringToInt)
printfn "%A" (co2 |> binaryStringToInt)
printfn "%A" lifeSupportRating
