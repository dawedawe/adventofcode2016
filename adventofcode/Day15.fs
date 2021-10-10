namespace Adventofcode2016

module Day15 =

    [<Literal>]
    let Input = "Day15Input.txt"

    type Disc =
        { Number: int
          Positions: int
          StartPos: int }

    let parseLine (s: string) =
        let regex =
            System.Text.RegularExpressions.Regex(@"\d+")

        let matches = regex.Matches(s)

        { Number = int matches.[0].Value
          Positions = int matches.[1].Value
          StartPos = int matches.[3].Value }

    let posAtTime buttonPressedAt disc =
        let movementDuration = buttonPressedAt + disc.Number
        (disc.StartPos + movementDuration) % disc.Positions

    let rec iterate sec discs =
        let positions = discs |> Array.map (posAtTime sec)

        let fallThrough =
            positions |> Array.forall (fun p -> p = 0)

        if fallThrough then
            sec
        else
            iterate (sec + 1) discs

    let day15 () =
        Input
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> iterate 0
