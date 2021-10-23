namespace Adventofcode2016

module Day21 =

    open System

    [<Literal>]
    let InputFile = "Day21Input.txt"

    type Operation =
        | SwapPos of int * int
        | SwapLetter of char * char
        | RotateLeft of int
        | RotateRight of int
        | RotateBased of char
        | Reverse of int * int
        | Move of int * int

    let parseLine (s: string) =
        match s with
        | _ when s.StartsWith("swap position") -> SwapPos((int (string s.[14])), (int (string s.[30])))
        | _ when s.StartsWith("swap letter") -> SwapLetter(s.[12], s.[26])
        | _ when s.StartsWith("rotate left") -> RotateLeft(int (string s.[12]))
        | _ when s.StartsWith("rotate right") -> RotateRight(int (string s.[13]))
        | _ when s.StartsWith("rotate based") -> RotateBased s.[35]
        | _ when s.StartsWith("reverse") -> Reverse((int (string s.[18])), (int (string s.[28])))
        | _ when s.StartsWith("move") -> Move((int (string s.[14])), (int (string s.[28])))
        | _ -> failwith "unknown operation"

    let doOperation op (s: char []) =
        match op with
        | SwapPos (x, y) ->
            let xChar, yChar = s.[x], s.[y]
            s.[y] <- xChar
            s.[x] <- yChar
            s
        | SwapLetter (x, y) ->
            let xIdx = Array.IndexOf(s, x)
            let yIdx = Array.IndexOf(s, y)
            let xChar, yChar = s.[xIdx], s.[yIdx]
            s.[xIdx] <- yChar
            s.[yIdx] <- xChar
            s
        | RotateLeft x ->
            let s' = Array.copy s
            let m = s.Length
            let idx =
                [| 0 .. m - 1 |]
                |> Array.map (fun i -> (((i - x) % m) + m) % m)
            for i in 0 .. m - 1 do
                s'.[idx.[i]] <- s.[i]
            s'
        | RotateRight x ->
            let s' = Array.copy s
            let m = s.Length
            let idx =
                [| 0 .. m - 1 |]
                |> Array.map (fun i -> (i + x) % m)
            for i in 0 .. m - 1 do
                s'.[idx.[i]] <- s.[i]
            s'
        | RotateBased x ->
            let idx = Array.IndexOf(s, x)
            let additional = if idx >= 4 then 1 else 0
            let rotations = 1 + idx + additional
            let s' = Array.copy s
            let m = s.Length
            let idx =
                [| 0 .. m - 1 |]
                |> Array.map (fun i -> (i + rotations) % m)
            for i in 0 .. m - 1 do
                s'.[idx.[i]] <- s.[i]
            s'
        | Reverse (x, y) ->
            let sub = s.[x..y] |> Array.rev
            let pre = if x > 0 then s.[0..x - 1] else Array.empty
            let post =
                if y < s.Length - 1 then
                    s.[y + 1..s.Length - 1]
                else
                    Array.empty
            Array.concat [ pre; sub; post ]
        | Move (x, y) ->
            let s' =
                seq {
                    for i in 0 .. s.Length - 1 do
                        if i <> x && i <> y then yield s.[i]
                        if i = y then
                            if x < y then
                                yield s.[i]
                                yield s.[x]
                            else
                                yield s.[x]
                                yield s.[i]
                }
                |> Seq.toArray
            s'

    let rec scramble s operations =
        match operations with
        | [||] -> s
        | _ ->
            let s' = doOperation operations.[0] s
            scramble s' (Array.tail operations)

    let day21 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> scramble ("abcdefgh".ToCharArray())
        |> String
