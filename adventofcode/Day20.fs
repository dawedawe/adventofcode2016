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

    let mergeRanges (sortedRanges: (int64 * int64) list) =
        let rec helper merged toDo =
            match merged, toDo with
            | [], r :: rest -> helper [ r ] rest
            | _, [] -> merged
            | _, (lower, upper) :: rest ->
                let (previousLower, previousUpper) = List.last merged
                if (lower <= previousUpper) then
                    let mergedToAdd = (min lower previousLower, max upper previousUpper)
                    let merged' =
                        merged
                        |> List.take (merged.Length - 1)
                        |> fun x -> List.append x [ mergedToAdd ]
                    helper merged' rest
                else
                    let merged' = List.append merged [ (lower, upper) ]
                    helper merged' rest

        helper List.empty sortedRanges

    let sumRanges mergedRanges =
        mergedRanges
        |> List.sumBy (fun (lower, upper) -> upper - lower + 1L)

    let countAllowed (sortedRanges: (int64 * int64) list) =
        let mergedRanges = mergeRanges sortedRanges
        let blocked = sumRanges mergedRanges
        4294967296L - blocked

    let day20Part2 () =
        Input
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.Split('-') |> fun a -> int64 a.[0], int64 a.[1])
        |> Array.sortBy fst
        |> List.ofArray
        |> countAllowed
