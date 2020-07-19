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
