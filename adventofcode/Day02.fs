namespace Adventofcode2016

module Day02 =

    [<Literal>]
    let InputFile = "Day02Input.txt"

    let getInput path =
        System.IO.File.ReadAllLines(path)

    let move pos (m: char) =
        match pos with
        | 1 -> match m with
               | 'D' -> 4
               | 'R' -> 2
               | _ -> 1
        | 2 -> match m with
               | 'D' -> 5
               | 'L' -> 1
               | 'R' -> 3
               | _ -> 2
        | 3 -> match m with
               | 'D' -> 6
               | 'L' -> 2
               | _ -> 3
        | 4 -> match m with
               | 'U' -> 1
               | 'D' -> 7
               | 'R' -> 5
               | _ -> 4
        | 5 -> match m with
               | 'U' -> 2
               | 'D' -> 8
               | 'L' -> 4
               | 'R' -> 6
               | _ -> failwith "unknown movement"
        | 6 -> match m with
               | 'U' -> 3
               | 'L' -> 5
               | 'D' -> 9
               | _ -> 6
        | 7 -> match m with
               | 'U' -> 4
               | 'R' -> 8
               | _ -> 7
        | 8 -> match m with
               | 'U' -> 5
               | 'L' -> 7
               | 'R' -> 9
               | _ -> 8
        | 9 -> match m with
               | 'U' -> 6
               | 'L' -> 8
               | _ -> 9
        | _ -> failwith "unknown position"


    let calcCode (instructions: string []) =
        let rec helper currentPos instrctionLine =
            match instrctionLine with
            | [] -> currentPos
            | m :: rest -> let currentPos' = move currentPos m
                           helper currentPos' rest
        instructions
        |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
        |> Array.fold (fun (s: int list) v -> 
                        let number = helper (List.last s) v
                        List.append s [helper (List.last s) v]) [5]
        |> List.skip 1

    let day02() =
        let instructions = getInput InputFile
        let code = calcCode instructions
        code
        |> List.fold (fun s v -> s + v.ToString()) System.String.Empty
