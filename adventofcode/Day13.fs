namespace Adventofcode2016

module Day13 =

    [<Literal>]
    let Input = 1364

    let calcSum (x, y) =
        x * x + 3 * x + 2 * x * y + y + y * y + Input

    let countBits (x: int) =
        let mutable n = x

        seq {
            for _ in 0 .. 31 do
                if (n &&& 0x01 = 0x01) then yield 1
                n <- n >>> 1
        }
        |> Seq.sum

    let (|Even|Odd|) (x: int) = if (x % 2 = 0) then Even else Odd

    let calcSpace =
        calcSum
        >> countBits
        >> function
            | Even _ -> '.'
            | Odd _ -> '#'

    type Path = (int * int) list

    let getNeighbours path =
        let (x, y) = List.last path

        let neighborPositions =
            seq {
                if x > 0 then yield (x - 1, y)
                yield (x + 1, y)
                if y > 0 then yield (x, y - 1)
                yield (x, y + 1)
            }

        neighborPositions
        |> Seq.filter (fun p -> not (List.contains p path) && calcSpace p = '.')

    let bfs target =
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable pathFound = None
        let start = List.singleton (1, 1)
        queue.Enqueue(start)

        while queue.Count > 0 && Option.isNone pathFound do
            let currentPath = queue.Peek()
            let currentPos = List.last currentPath

            if currentPos = target then
                pathFound <- Some currentPath
            else
                queue.Dequeue() |> ignore
                let neighbors = getNeighbours currentPath

                for n in neighbors do
                    let currentPath' = List.append currentPath [ n ]
                    queue.Enqueue(currentPath')

        match pathFound with
        | Some path -> path.Length - 1
        | None -> failwith "no path found"


    let day13 () = bfs (31, 39)
