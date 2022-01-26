#load "../fileReader.fsx"
open AOC.AOCFileReader

let isOpening char =
    if char = "[" || char = "(" || char = "{" || char = "<" 
    then true
    else false

let matchingChar char =
    match char with
    | "]" -> "["
    | ")" -> "("
    | "}" -> "{"
    | ">" -> "<"
    | _ -> "-"

let scorePoints char =
    match char with
    | "]" -> 57
    | ")" -> 3
    | "}" -> 1197
    | ">" -> 25137
    | "(" -> 1
    | "[" -> 2
    | "{" -> 3
    | "<" -> 4
    | _ -> 0

let lines =
    readFileAsStringLines "./FS/day10/input.txt"
    |> Array.map (fun l -> Seq.toList l |> List.map (fun c -> c.ToString()))

let removeAt index input =
    input
    |> List.mapi (fun i el -> (i <> index, el))
    |> List.filter fst
    |> List.map snd

let rec processSingleLine (line: list<string>) (stack: list<string>) =
    match line with
    | head :: tail ->
        let opening = isOpening head
        if opening then processSingleLine tail (List.append stack [ head ])
        else
            let matching = matchingChar head
            let isMatching = stack.[(stack.Length - 1)].Equals(matching)
            if not isMatching then head
            else processSingleLine tail (removeAt (stack.Length - 1) stack)
    | _ -> "OK"

let result =
    lines
    |> Array.map (fun l -> scorePoints (processSingleLine l []))
    |> Array.sum

printf "%A" result

//2.
let incompleteLines = lines |> Array.filter (fun l -> (processSingleLine l []) = "OK")

let rec getIncompleteChars (line: list<string>) (stack: list<string>) =
    match line with
    | head :: tail ->
        let opening = isOpening head
        if opening then getIncompleteChars tail (List.append stack [ head ])
        else getIncompleteChars tail (removeAt (stack.Length - 1) stack)
    | _ -> stack

let reverseList list =
    list
    |> List.mapi (fun i el -> (i, el))
    |> List.sortByDescending (fun el -> fst el)
    |> List.map (fun el -> snd el)

let rec scoreIncomplete chars (score:System.Numerics.BigInteger) =
    match chars with
    | head :: tail -> scoreIncomplete tail ((score * (bigint 5)) + bigint (scorePoints head))
    | _ -> score

let charsToComplete =
    incompleteLines
    |> Array.map (fun l -> reverseList (getIncompleteChars l []))
    |> Array.map (fun l -> scoreIncomplete l (bigint 0))
    |> Array.sort

let result2 = charsToComplete.[((charsToComplete.Length / 2))]
printf "%A" result2