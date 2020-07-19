namespace Adventofcode2016

module Day02 =

    [<Literal>]
    let InputFile = "Day02Input.txt"

    let getInput path =
        System.IO.File.ReadAllLines(path)

    let move pos (m: char) =
        match pos with
        | 1 ->
            match m with
            | 'D' -> 4
            | 'R' -> 2
            | _ -> 1
        | 2 ->
            match m with
            | 'D' -> 5
            | 'L' -> 1
            | 'R' -> 3
            | _ -> 2
        | 3 ->
            match m with
            | 'D' -> 6
            | 'L' -> 2
            | _ -> 3
        | 4 ->
            match m with
            | 'U' -> 1
            | 'D' -> 7
            | 'R' -> 5
            | _ -> 4
        | 5 ->
            match m with
            | 'U' -> 2
            | 'D' -> 8
            | 'L' -> 4
            | 'R' -> 6
            | _ -> failwith "unknown movement"
        | 6 ->
            match m with
            | 'U' -> 3
            | 'L' -> 5
            | 'D' -> 9
            | _ -> 6
        | 7 ->
            match m with
            | 'U' -> 4
            | 'R' -> 8
            | _ -> 7
        | 8 ->
            match m with
            | 'U' -> 5
            | 'L' -> 7
            | 'R' -> 9
            | _ -> 8
        | 9 ->
            match m with
            | 'U' -> 6
            | 'L' -> 8
            | _ -> 9
        | _ -> failwith "unknown position"


    let calcCode (instructions: string []) start moveF =
        let rec helper currentPos instrctionLine =
            match instrctionLine with
            | [] -> currentPos
            | m :: rest ->
                let currentPos' = moveF currentPos m
                helper currentPos' rest
        instructions
        |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
        |> Array.fold (fun s v -> List.append s [ helper (List.last s) v ]) [ start ]
        |> List.skip 1

    let day02() =
        let instructions = getInput InputFile
        let code = calcCode instructions 5 move
        code |> List.fold (fun s v -> s + v.ToString()) System.String.Empty

    let movePart2 (pos: char) (m: char) =
        match pos with
        | '1' ->
            match m with
            | 'D' -> '3'
            | _ -> '1'
        | '2' ->
            match m with
            | 'R' -> '3'
            | 'D' -> '6'
            | _ -> '2'
        | '3' ->
            match m with
            | 'U' -> '1'
            | 'L' -> '2'
            | 'R' -> '4'
            | 'D' -> '7'
            | _ -> failwith "unknown movement"
        | '4' ->
            match m with
            | 'L' -> '3'
            | 'D' -> '8'
            | _ -> '4'
        | '5' ->
            match m with
            | 'R' -> '6'
            | _ -> '5'
        | '6' ->
            match m with
            | 'U' -> '2'
            | 'L' -> '5'
            | 'R' -> '7'
            | 'D' -> 'A'
            | _ -> failwith "unknown movement"
        | '7' ->
            match m with
            | 'U' -> '3'
            | 'R' -> '8'
            | 'D' -> 'B'
            | 'L' -> '6'
            | _ -> failwith "unknown movement"
        | '8' ->
            match m with
            | 'U' -> '4'
            | 'R' -> '9'
            | 'D' -> 'C'
            | 'L' -> '7'
            | _ -> failwith "unknown movement"
        | '9' ->
            match m with
            | 'L' -> '8'
            | _ -> '9'
        | 'A' ->
            match m with
            | 'U' -> '6'
            | 'R' -> 'B'
            | _ -> 'A'
        | 'B' ->
            match m with
            | 'U' -> '7'
            | 'R' -> 'C'
            | 'D' -> 'D'
            | 'L' -> 'A'
            | _ -> failwith "unknown movement"
        | 'C' ->
            match m with
            | 'U' -> '8'
            | 'L' -> 'B'
            | _ -> 'C'
        | 'D' ->
            match m with
            | 'U' -> 'B'
            | _ -> 'D'
        | _ -> failwith "unknown position"

    let day02Part2() =
        let instructions = getInput InputFile
        let code = calcCode instructions '5' movePart2
        code |> List.fold (fun s v -> s + v.ToString()) System.String.Empty
