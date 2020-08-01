namespace Adventofcode2016

module Day08 =

    [<Literal>]
    let InputFile = "Day08Input.txt"

    type Instruction =
        | Rect of (int * int)
        | RotRow of (int * int)
        | RotCol of (int * int)

    let parseRect (s: string) =
        let s' = s.Remove(0, 5)
        let xIndex = s'.IndexOf('x')
        let width = int s'.[0..xIndex - 1]
        let height = int s'.[xIndex + 1..]
        Rect(width, height)

    let parseRotation (s: string) =
        let equalIndex = s.IndexOf('=')
        let byIndex = s.IndexOf(" by")
        let index = s.Substring(equalIndex + 1, byIndex - equalIndex - 1) |> int
        let shift = int s.[byIndex + 4..]
        (index, shift)

    let parseInstruction (s: string) =
        match s with
        | _ when s.StartsWith("rect") -> parseRect s
        | _ when s.StartsWith("rotate row") -> parseRotation s |> RotRow
        | _ when s.StartsWith("rotate column") -> parseRotation s |> RotCol
        | _ -> failwith "unknown instruction"

    let getInstructions path =
        path
        |> System.IO.File.ReadAllLines
        |> Array.map parseInstruction

    let applyRect (w, h) (display: char [,]) =
        for row in 0 .. h - 1 do
            for col in 0 .. w - 1 do
                display.[row, col] <- '#'

    let applyRotCol (col, shift) (display: char [,]) =
        let origCol = Array.copy display.[*, col]
        for r in 0 .. (origCol.Length - 1) do
            let r' = (r + shift) % origCol.Length
            display.[r', col] <- origCol.[r]

    let applyRotRow (row, shift) (display: char [,]) =
        let origRow = Array.copy display.[row, *]
        for c in 0 .. (origRow.Length - 1) do
            let c' = (c + shift) % origRow.Length
            display.[row, c'] <- origRow.[c]

    let apply (display: char [,]) instruction =
        match instruction with
        | Rect p -> applyRect p display
        | RotCol p -> applyRotCol p display
        | RotRow p -> applyRotRow p display

    let day08() =
        let h = 6
        let w = 50
        let display = Array2D.create h w '.'
        getInstructions InputFile |> Array.iter (apply display)
        seq {
            for r in 0 .. h - 1 do
                for c in 0 .. w - 1 do
                    if display.[r, c] = '#' then yield 1 else yield 0
        }
        |> Seq.sum
