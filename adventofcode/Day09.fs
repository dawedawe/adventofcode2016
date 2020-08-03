namespace Adventofcode2016

module Day09 =

    [<Literal>]
    let InputFile = "Day09Input.txt"

    let repeat times (s: string) =
        let mutable output = ""
        for i in 1 .. times do
            output <- output + s
        output

    let decompress (s: string) =

        let rec helper (todo: string) (sofar: string) =
            let openBracket = todo.IndexOf('(')
            let closeBracket = todo.IndexOf(')')
            if (openBracket >= 0 && closeBracket > 0) then
                let sofar' =
                    sofar + if (openBracket > 0) then todo.[0..(openBracket - 1)] else ""

                let marker = todo.Substring(openBracket + 1, closeBracket - openBracket - 1)
                let xIdx = marker.IndexOf('x')
                let toTake = marker.Substring(0, xIdx) |> int
                let times = marker.Substring(xIdx + 1) |> int
                let part = todo.[(closeBracket + 1)..(closeBracket + 1 + toTake - 1)] |> repeat times
                let sofar'' = sofar' + part
                let todo' = todo.[(closeBracket + 1 + toTake)..]
                helper todo' sofar''
            else
                let sofar' = sofar + todo
                sofar'

        helper s ""

    let day09() =
        let input = System.IO.File.ReadAllText(InputFile)
        let decompressed = decompress input
        decompressed.Length

    let decompressPart2 (s: string) =

        let rec helper (todo: string) (sofar: int64) =
            match todo with
            | "" -> sofar
            | _ when not (todo.StartsWith("(")) -> helper todo.[1..] (sofar + 1L)
            | _ when todo.StartsWith("(") ->
                let openBracket = todo.IndexOf('(')
                let closeBracket = todo.IndexOf(')')
                let marker = todo.Substring(openBracket + 1, closeBracket - openBracket - 1)
                let xIdx = marker.IndexOf('x')
                let toTake = marker.Substring(0, xIdx) |> int
                let times = marker.Substring(xIdx + 1) |> int
                let part = todo.[(closeBracket + 1)..(closeBracket + 1 + toTake - 1)] |> repeat times
                if part.Contains("(") then
                    let sofar' = sofar
                    let todo' = part + todo.[(closeBracket + 1 + toTake)..]
                    helper todo' sofar'
                else
                    let sofar' = sofar + (int64 part.Length)
                    let todo' = todo.[(closeBracket + 1 + toTake)..]
                    helper todo' sofar'
            | _ -> failwith "bad state"

        helper s 0L

    let day09Part2() =
        let input = System.IO.File.ReadAllText(InputFile)
        let decompressed = decompressPart2 input
        decompressed
