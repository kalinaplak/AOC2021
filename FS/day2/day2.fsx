#load "../fileReader.fsx"

open AOC.AOCFileReader

let mapTupleToPosition =
    fun tuple ->
        match tuple with
        | (direction, num) ->
            match direction with
            | "forward" -> (num, 0)
            | "down" -> (0, num)
            | "up" -> (0, -num)
            | _ -> (0, 0)

//cz.1
let addTuples a b = fst a + fst b, snd a + snd b
//cz.2
let addTupleToPosition (h, d, a) (th, ta) =
    (h + th), (d + (if th <> 0 then a * th else 0)), (a + ta)

let calculateFinalPosition tupleSeq fn initial =
    tupleSeq
    |> Seq.toList
    |> List.fold (fun acc el -> fn acc el) initial

let tuplesSeq =
    (readFileAsStringLines "./FS/day2/input.txt", " ")
    |> splitLinesToStringIntTuples
    |> Seq.map mapTupleToPosition

printfn "%A" tuplesSeq

let finalPosition =
    calculateFinalPosition tuplesSeq addTuples (0, 0)

let result = fst finalPosition * snd finalPosition
printfn "%A" result

let (horizontal, depth, aim) =
    calculateFinalPosition tuplesSeq addTupleToPosition (0, 0, 0)

let result2 = horizontal * depth
printfn "%A" result2
