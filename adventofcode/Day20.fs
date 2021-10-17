namespace Adventofcode2016

module Day20 =

    [<Literal>]
    let Input = "Day20Input.txt"

    let findMin sortedRanges =
        let rec helper min ranges =
            match ranges with
            | [||] -> min
            | _ ->
                let lower, upper = Array.head ranges
                let min' = if lower <= min && min <= upper then upper + 1L else min
                helper min' (Array.tail ranges)
        helper 0L sortedRanges

    let day20 () =
        Input
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.Split('-') |> fun a -> int64 a.[0], int64 a.[1])
        |> Array.sortBy fst
        |> findMin
