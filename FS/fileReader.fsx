namespace AOC

module AOCFileReader =
    let readFileAsStringLines =
        fun path -> (System.IO.File.ReadAllText path).Split "\n"
    
    let readFileAsNumbers =
        fun path ->
            readFileAsStringLines path
            |> Seq.filter (fun x -> x <> "")
            |> Seq.map (fun x -> x |> int)

    let splitLinesToStringIntTuples =
        fun (list: string [], separator: string) ->
            list
            |> Seq.map (fun x -> x.Split separator)
            |> Seq.map (fun x -> (x.[0], x.[1]))
            |> Seq.map (fun (d, n) -> (d, n |> int))