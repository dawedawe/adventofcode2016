namespace Adventofcode2016

module Day03 =

    [<Literal>]
    let InputFile = "Day03Input.txt"

    let getInput path =
        System.IO.File.ReadAllLines(path)
        |> Array.map (fun s ->
            s.Split([| ' ' |])
            |> Array.filter ((<>) "")
            |> Array.map int)

    let isLegal (lengths: int array) =
        let (a, b, c) = lengths.[0], lengths.[1], lengths.[2]
        a + b > c && a + c > b && b + c > a

    let day03() =
        getInput InputFile
        |> Array.filter isLegal
        |> Array.length

    let day03Part2() =
        let lines = getInput InputFile
        let tris = seq {
            for i in [0 .. 3 .. (lines.Length - 3)] do
                let line0 = lines.[i]
                let line1 = lines.[i+1]
                let line2 = lines.[i+2]
                let tri0 = [| line0.[0]; line1.[0]; line2.[0]  |]
                let tri1 = [| line0.[1]; line1.[1]; line2.[1]  |]
                let tri2 = [| line0.[2]; line1.[2]; line2.[2]  |]
                [| tri0; tri1; tri2 |]
        }
        tris
        |> Array.ofSeq
        |> Array.collect id
        |> Array.filter isLegal
        |> Array.length
