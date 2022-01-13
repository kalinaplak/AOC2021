#load "../fileReader.fsx"

open AOC.AOCFileReader
open System.Numerics

let numbers = (readFileAsNumbersAndSplit "./FS/day7/input.txt") |> Array.toList

printf "%A" numbers

let rec getMinMaxValues numbers min max =
    match numbers with
    | head :: tail ->
        if head < min then getMinMaxValues tail head max
        else if head > max then getMinMaxValues tail min head
        else getMinMaxValues tail min max
    | _ -> (min,max)

let (minPos, maxPos) = getMinMaxValues numbers 999999999 0
printf "%A" (minPos, maxPos)

let getFuelCost numbers position =
    numbers 
    |> List.map (fun n -> abs (n - position))
    |> List.sum

let rec getBestFuelCost costFn numbers pos maxPos cost =
    if(pos <= maxPos) 
    then 
        let newCost = costFn numbers pos
        if newCost < cost then getBestFuelCost costFn numbers (pos+1) maxPos newCost
        else getBestFuelCost costFn numbers (pos+1) maxPos cost
    else
        cost

let cost = getBestFuelCost getFuelCost numbers minPos maxPos 999999999
printf "%A" cost

let getFuelCostNumber n =
  let rec getFuelCostR acc i =
    if i <= n then getFuelCostR (acc + i) (i+1) else acc
  getFuelCostR 0 1

let getFuelCost2 numbers position =
    numbers 
    |> List.map (fun n -> getFuelCostNumber (abs (n - position)))
    |> List.sum

let cost2 = getBestFuelCost getFuelCost2 numbers minPos maxPos 999999999
printf "%A" cost2
