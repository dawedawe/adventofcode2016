namespace Adventofcode2016

module Day24 =

    [<Literal>]
    let InputFile = "Day24Input.txt"

    type Path = (int * int) list

    let isValidPos (map: string []) (posX, posY) =
        posX >= 0
        && posX < map.[0].Length
        && posY >= 0
        && posY < map.Length
        && map.[posY].[posX] <> '#'

    let getNeighbours (queueDump: Path []) path (posX, posY) map =
        let allVisited = queueDump |> Seq.collect id
        seq {
            yield (posX, posY - 1)
            yield (posX + 1, posY)
            yield (posX, posY + 1)
            yield (posX - 1, posY)
        }
        |> Seq.filter (fun p ->
            isValidPos map p
            && not (List.contains p path)
            && not (Seq.contains p allVisited))

    let bfs map source target =
        let queue = System.Collections.Generic.Queue<Path>()
        let mutable pathFound = None
        let start = List.singleton source
        queue.Enqueue(start)

        while queue.Count > 0 && Option.isNone pathFound do
            let currentPath = queue.Peek()
            let currentPos = List.last currentPath

            if currentPos = target then
                pathFound <- Some currentPath
            else
                queue.Dequeue() |> ignore
                let queueDump = queue.ToArray()
                let neighbors = getNeighbours queueDump currentPath currentPos map

                for n in neighbors do
                    let currentPath' = List.append currentPath [ n ]
                    queue.Enqueue(currentPath')

        match pathFound with
        | Some path -> path.Length - 1
        | None -> failwith "no path found"

    let getPositions (map: string []) =
        seq {
            for x in 0 .. map.[0].Length - 1 do
                for y in 0 .. map.Length - 1 do
                    if System.Char.IsNumber(map.[y].[x]) then (x, y)
        }
        |> Seq.toList

    let getAllPairs (positions: (int * int) list) =
        seq {
            for p1 in positions do
                for p2 in positions do
                    if p1 <> p2 then yield [ p1; p2 ] |> Set.ofList
        }
        |> Set.ofSeq

    type Node =
        { Current: char
          Distance: int
          Visited: Set<char>
          Path: (char * char) list }

    type Way = { V1: char; V2: char; Distance: int }

    let getNextPossibleStates currentState distances =
        distances
        |> Seq.filter (fun w ->
            (currentState.Current = w.V1)
            && not (List.contains (w.V1, w.V2) currentState.Path))

    let bfs2 distances toVisit =
        let mutable candidate: Node option = None
        let queue = System.Collections.Generic.Queue<Node>()
        let start =
            { Current = '0'
              Path = List.empty
              Distance = 0
              Visited = Set.singleton '0' }
        queue.Enqueue(start)

        while queue.Count > 0 do
            let currentNode = queue.Peek()

            if currentNode.Visited.Count = toVisit then
                if Option.isNone candidate
                   || candidate.Value.Distance > currentNode.Distance then
                    candidate <- Some currentNode
                queue.Dequeue() |> ignore
            else
                let currentBest =
                    match candidate with
                    | Some c -> c.Distance
                    | None -> System.Int32.MaxValue
                queue.Dequeue() |> ignore
                let neighbors = getNextPossibleStates currentNode distances
                for n in neighbors do
                    let successorNode =
                        { currentNode with
                            Current = n.V2
                            Path = List.append currentNode.Path [ (currentNode.Current, n.V2) ]
                            Distance = currentNode.Distance + n.Distance
                            Visited = Set.add n.V2 currentNode.Visited }
                    if successorNode.Distance < currentBest then
                        queue.Enqueue(successorNode)

        match candidate with
        | Some c -> c.Distance
        | None -> failwith "no path found"

    let day24 () =
        let map = InputFile |> System.IO.File.ReadAllLines
        let positions = getPositions map
        let pairs = getAllPairs positions
        let distances =
            seq {
                for pair in pairs do
                    let (sourceX, sourceY), (targetX, targetY) = pair |> Set.toList |> fun l -> l.[0], l.[1]
                    let path = bfs map (sourceX, sourceY) (targetX, targetY)
                    yield
                        { V1 = map.[sourceY].[sourceX]
                          V2 = map.[targetY].[targetX]
                          Distance = path }
                    yield
                        { V2 = map.[sourceY].[sourceX]
                          V1 = map.[targetY].[targetX]
                          Distance = path }
            }
            |> Seq.toList
        bfs2 distances positions.Length
