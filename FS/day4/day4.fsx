#load "../fileReader.fsx"

open AOC.AOCFileReader

let inputFile =
    readFileAsStringLines "./FS/day4/input.txt"
    |> Array.filter (fun l -> l <> "")

let bingoNumbers = inputFile.[0].Split(",") |> Array.toList 

let mapBoardInput (boardRaw: list<string>) =
        boardRaw
        |> List.map (fun line -> line.Trim().Replace("  ", " ").Split(" "))
        |> List.map (fun row -> row |> Array.toList)

type Field = { value: string; isChecked: bool }
type Board = { rows: List<List<Field>>; columns: List<List<Field>> }

let getEmptyBoard = { rows = List.empty; columns = List.empty }

let mapArrToFields (boardArr: list<list<string>>) =
        boardArr
        |> List.map (fun row -> row |> List.map (fun v -> { value = v; isChecked = false }))

let rec createBoard (boardArr: list<list<Field>>, i, j, board: Board) =
        if (i = boardArr.Length) then
            board
        else if (j = boardArr.[i].Length) then
            createBoard (boardArr, (i + 1), 0, board)
        else
            let row = boardArr.Item(i)
            let item = row.Item(j)
            let cols = if board.columns.IsEmpty then (row |> List.map (fun r -> [])) else board.columns

            let newBoard = { 
                rows = if j = 0 then List.append board.rows [ row ] else  board.rows
                columns = cols |> List.mapi (fun i c -> if i <> j then c else List.append (cols.Item(j)) [ item ]) 
            }
            createBoard (boardArr, i, (j + 1), newBoard)

let mapToBoards (boardArr: list<list<list<Field>>>) =
        boardArr
        |> List.map (fun board ->
            createBoard ( board, 0, 0, getEmptyBoard))
    
let checkNumberInRows (rows: list<list<Field>>, number: string) =
    rows |> List.map (fun r -> r |> List.map(fun (item: Field)-> if item.value = number then { value = item.value; isChecked = true } else item ))

let checkNumberInBoard (board: Board, number: string) =
    let rows = checkNumberInRows (board.rows,number)
    let columns = checkNumberInRows (board.columns, number)
    {  rows = rows; columns = columns }

let checkNumberInAllBoards (boards: list<Board>, number: string) =
    boards |> List.map (fun b -> checkNumberInBoard(b, number))

let checkWinningInRows (rows: list<list<Field>>) =
    rows |> List.tryFindIndex (fun (row: list<Field>) -> row |> List.forall (fun el -> el.isChecked) )

let checkWinningInBoard (board: Board) =
    if (checkWinningInRows board.rows) <> None then true else 
        if (checkWinningInRows board.columns) <> None then true else false

let getUncheckedValuesSumInRow (rows: list<Field>) =
    rows |> List.fold (fun acc curr -> if curr.isChecked = false then acc + (curr.value |> int) else acc) 0

let getUncheckedValuesSumInBoard (board:Board) =
    board.rows |> List.fold (fun acc curr ->  acc + (getUncheckedValuesSumInRow curr)) 0

let rec applyNumbers (boards: list<Board>, numbers: list<string>) =
        match numbers with
            | head::tail -> 
                let newBoards = checkNumberInAllBoards(boards, head)
                let winningBoard = newBoards |> List.tryFind (fun b -> checkWinningInBoard(b))
                if winningBoard <> None then (winningBoard, head) else applyNumbers (newBoards, tail)
            | _ -> (None, "")
    
let boardsArrays =
    (inputFile
    |> Array.toList
    |> List.skip (1)
    |> List.chunkBySize 5
    |> List.map mapBoardInput
    |> List.map mapArrToFields)
    |> mapToBoards

// 1.
let getResult (boards, numbers) =
    let (winningBoard, winningNumber) = applyNumbers (boards, numbers)
    let uncheckedSum = 
        match winningBoard with
        | Some(winningBoard) -> getUncheckedValuesSumInBoard winningBoard
        | None -> 0
    uncheckedSum * (winningNumber |> int)

let result = getResult(boardsArrays, bingoNumbers)
printfn "%A" result

// 2.
let filterNotWinningBoards boards = 
    boards |> List.filter (fun b -> checkWinningInBoard(b) = false)

let rec applyAllNumbers (boards: list<Board>, numbers: list<string>, lastWinningBoard: Board, lastWinningNumber: string) =
        match numbers with
            | head::tail -> 
                let newBoards = checkNumberInAllBoards(boards, head)
                let winningIndex = newBoards |> List.tryFindIndex (fun b -> checkWinningInBoard(b))
                match winningIndex with 
                | Some(winningIndex) -> 
                    let notWinningBoards = filterNotWinningBoards newBoards
                    applyAllNumbers(notWinningBoards, tail, newBoards.Item(winningIndex), head)
                | None -> applyAllNumbers(newBoards, tail, lastWinningBoard, lastWinningNumber)
            | _ -> (lastWinningBoard,lastWinningNumber)

let getLastResult (boards, numbers) =
    let (winningBoard, winningNumber) = applyAllNumbers (boards, numbers, getEmptyBoard, "")
    let uncheckedSum = getUncheckedValuesSumInBoard winningBoard
    uncheckedSum * (winningNumber |> int)

let result2 = getLastResult(boardsArrays, bingoNumbers)
printfn "%A" result2
