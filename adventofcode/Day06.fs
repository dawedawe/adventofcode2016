namespace Adventofcode2016

module Day06 =

    [<Literal>]
    let InputFile = "Day06Input.txt"

    let getInput path =
        path
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())

    let decode f (lines: char [] []) =
        let length = lines.[0].Length

        let word =
            seq {
                for p in [ 0 .. (length - 1) ] do
                    let maxGroup =
                        lines
                        |> Array.map (fun l -> l.[p])
                        |> Array.groupBy id
                        |> f (fun (_, g) -> Array.length g)
                    yield (fst maxGroup)
            }
        word |> Seq.fold (fun s c -> s + string c) ""

    let day06() =
        let lines = getInput InputFile
        let word = decode Array.maxBy lines
        word

    let day06Part2() =
        let lines = getInput InputFile
        let word = decode Array.minBy lines
        word
