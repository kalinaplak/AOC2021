#load "../fileReader.fsx"

open AOC.AOCFileReader

let numbers =
    readFileLinesAsNumbers "./FS/day6/input.txt" |> Array.toList

let gropuPopulationToMap population =
    Map(population |> List.groupBy id |> List.map (fun (c, cs) -> c, List.length cs))

let applyGrowthToTuple (tuple: (int * int)) = 
    let (value,count) = tuple
    match value with
    | 0 -> [(6,count); (8,count)]
    | num -> [((num - 1), count)]

let rec appendListToPopulationMap list (map:Map<int,int>) =
    match list with
    | head:: tail ->
        let newMap = 
            if map.ContainsKey (fst head) 
            then 
                let currCount = map.Item(fst head)
                map.Add(fst head, (currCount + snd head))
            else
                map.Add(fst head, snd head)
        appendListToPopulationMap tail newMap
    | _ -> map

let rec simulatePopulationGrowth population newPopulation day maxDay =
    let pLis = Map.toList population
    match pLis with
    | head::tail -> 
        let newPop = applyGrowthToTuple head
        let map = appendListToPopulationMap newPop newPopulation
        simulatePopulationGrowth (Map(tail)) map day maxDay
    | _ -> 
        if day < maxDay  
        then simulatePopulationGrowth newPopulation Map.empty (day + 1) maxDay 
        else newPopulation

let group = gropuPopulationToMap numbers
let result = simulatePopulationGrowth group Map.empty 1 80
let length = (Map.toList result) |> (List.fold (fun acc curr -> acc + (snd curr)) 0)
printf "%A" length