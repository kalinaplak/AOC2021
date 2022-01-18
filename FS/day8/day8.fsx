#load "../fileReader.fsx"

open AOC.AOCFileReader
open System.Numerics

let numbers =
    (readFileAsStringLines "./FS/day8/input.txt")
    |> Array.map (fun l ->
        l.Split(" | ")
        |> Array.map (fun l -> l.Split(" ")))
    |> Array.map (fun l -> (l.[0], l.[1]))

let result1 =
    numbers
    |> Array.map (fun l ->
        snd l
        |> Array.map (fun c -> c.Length)
        |> Array.filter (fun len -> len = 2 || len = 4 || len = 3 || len = 7))
    |> Array.sumBy (fun el -> el.Length)

printf "%A" result1

let diff a b =
    let aList = Seq.toList a
    let bList = Seq.toList b
    let chars = aList |> List.except bList
    chars |> List.map string |> List.reduce (+)

let findByLength list length =
    list |> Array.find (fun (el: string) -> el.Length = length)

let sortString el =
    Seq.toList el |> Seq.sort |> Seq.map string |> Seq.reduce (+)

let except a1 a2 =
    let sorted1 = a1 |> Array.map(fun el -> sortString el)
    let sorted2 = a2 |> Array.map(fun el -> sortString el)
    sorted1 |> Array.except sorted2

let calculateLetters list =
    let one = findByLength list 2
    let seven = findByLength list 3
    let eight = findByLength list 6
    let four = findByLength list 4
    let exceptFoundNum = except list [|one; seven; eight; four;|]
    let a = diff seven one
    let removedOne = exceptFoundNum |> Array.map(fun el -> diff el one)
    let three = (findByLength removedOne 3) + one
    let b = diff four three
    let d = diff (diff four one) b
    let exceptThree = except exceptFoundNum [|three;|]
    let removedABD = exceptThree |> Array.map(fun el -> diff (diff (diff el a) b) d)
    let five = (findByLength removedABD 2) + a + b + d
    let zero = (findByLength removedABD 4) + a + b
    let exceptFiveAndZero = except exceptThree [|five; zero|]
    let two = findByLength exceptFiveAndZero 5
    let g = diff (diff (diff (diff five a) b) d) one
    let e = diff (diff (diff zero seven) b) g
    let c = diff (diff two five) e
    let f = diff one c
    (a,b,c,d,e,f,g)

let calculateNumber elem (a:string,b:string,c:string,d:string,e:string,f:string,g: string) =
    let elSorted = sortString elem
    if elSorted = (sortString (a+b+c+e+f+g)) then 0
    else if elSorted = (sortString (c+f)) then 1
    else if elSorted = (sortString (a+c+d+e+g)) then 2
    else if elSorted = (sortString (a+c+d+f+g)) then 3
    else if elSorted = (sortString (b+c+d+f)) then 4
    else if elSorted = (sortString (a+b+d+f+g)) then 5
    else if elSorted = (sortString (a+b+d+e+f+g)) then 6
    else if elSorted = (sortString (a+c+f)) then 7
    else if elSorted = (sortString (a+b+c+d+e+f+g)) then 8
    else if elSorted = (sortString (a+b+c+d+f+g)) then 9
    else -1

let calucalteNumbers elemArr letters =
    let numbers = elemArr |> Array.map (fun el -> calculateNumber el letters)
    (numbers.[0] * 1000) + (numbers.[1] * 100) + (numbers.[2] * 10) + numbers.[3]

let calculateResult input =
    input |> Array.map(fun i -> 
        let data = fst i
        let numbers = snd i
        let letters = calculateLetters data
        calucalteNumbers numbers letters
    ) |> Array.sum

let result2 = calculateResult numbers