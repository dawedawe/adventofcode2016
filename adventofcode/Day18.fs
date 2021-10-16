namespace Adventofcode2016

module Day18 =

    [<Literal>]
    let Input = "Day18Input.txt"

    let nextRow (previousRow: string) i =
        let left =
            if i = 0 then
                '.'
            else
                previousRow.[i - 1]

        let right =
            if i = (String.length previousRow - 1) then
                '.'
            else
                previousRow.[i + 1]

        let center = previousRow.[i]

        match (left, center, right) with
        | '^', '^', '.' -> '^'
        | '.', '^', '^' -> '^'
        | '^', '.', '.' -> '^'
        | '.', '.', '^' -> '^'
        | _ -> '.'

    let countSafeTiles rowCount line =
        let rec helper rowsLeft previousRow safeTiles =
            if rowsLeft = 0 then
                safeTiles
            else
                let nextRow =
                    [ 0 .. (String.length previousRow - 1) ]
                    |> Seq.map (nextRow previousRow)
                    |> Seq.toArray
                    |> System.String

                let safeTilesInRow =
                    nextRow
                    |> Seq.sumBy (fun c -> if c = '.' then 1 else 0)

                let safeTiles' = safeTiles + safeTilesInRow
                let rowsLeft' = rowsLeft - 1
                helper rowsLeft' nextRow safeTiles'

        let safeTilesInRow =
            line
            |> Seq.sumBy (fun c -> if c = '.' then 1 else 0)

        helper rowCount line safeTilesInRow

    let day18 () =
        Input
        |> System.IO.File.ReadAllText
        |> countSafeTiles 39
