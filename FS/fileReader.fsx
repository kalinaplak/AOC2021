namespace AOC

module AOCFileReader =
    let readFileAsStringLines path =
        (System.IO.File.ReadAllText path).Split "\n"

    let readFileLinesAsBigIntNumbers path =
        (readFileAsStringLines path)
        |> Array.map (fun l -> l.Split(",") |> Array.map (fun s -> s |> System.Numerics.BigInteger.Parse)) 
        |> Array.collect id

    let readFileAsNumbers path =
        readFileAsStringLines path
        |> Seq.filter (fun x -> x <> "")
        |> Seq.map (fun x -> x |> int)

    let readFileAsNumbersAndSplit path =
        readFileAsStringLines path
        |> Array.map (fun l -> l.Split(",") |> Array.map (fun s -> s |> int)) 
        |> Array.collect id

    let splitLinesToStringIntTuples (list: string [], separator: string) =
        list
        |> Seq.map (fun x -> x.Split separator)
        |> Seq.map (fun x -> (x.[0], x.[1]))
        |> Seq.map (fun (d, n) -> (d, n |> int))
