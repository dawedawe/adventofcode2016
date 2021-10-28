namespace Adventofcode2016

module Day22 =

    [<Literal>]
    let InputFile = "Day22Input.txt"

    type Node =
        { X: int
          Y: int
          Size: int
          Used: int
          Avail: int }

    let parse (s: string) =
        let r =
            System.Text.RegularExpressions.Regex(@"/dev/grid/node-x(\d+)-y(\d+)(\s*)(\d+)T(\s*)(\d+)T(\s*)(\d+)T")
        let m = r.Match(s)
        { X = int m.Groups.[1].Value
          Y = int m.Groups.[2].Value
          Size = int m.Groups.[4].Value
          Used = int m.Groups.[6].Value
          Avail = int m.Groups.[8].Value }

    let isPair n1 n2 =
        n1.Used > 0 && n1 <> n2 && n1.Used <= n2.Avail

    let getPairs node nodes =
        nodes |> Array.filter (fun n -> isPair node n)

    let day22 () =
        let nodes =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.skip 2
            |> Array.map parse
        nodes
        |> Array.collect (fun n -> getPairs n nodes)
        |> Array.length

    let canMove source target = target.Size >= source.Used

    let move nodeMap source target =
        nodeMap
        |> Map.add target { nodeMap.[target] with Used = nodeMap.[source].Used }
        |> Map.add source { nodeMap.[source] with Used = 0 }

    let rec moveEmptyNodeToFirstRow moves (nodeMap: Map<int * int, Node>) emptyNode =
        if snd emptyNode = 0 then
            moves, nodeMap
        else
            let up = (fst emptyNode, snd emptyNode - 1)
            if canMove nodeMap.[up] nodeMap.[emptyNode] then
                let nodeMap' = move nodeMap up emptyNode
                let moves' = moves + 1
                moveEmptyNodeToFirstRow moves' nodeMap' up
            else
                let left = (fst emptyNode - 1, snd emptyNode)
                if canMove nodeMap.[left] nodeMap.[emptyNode] then
                    let nodeMap' = move nodeMap left emptyNode
                    let moves' = moves + 1
                    moveEmptyNodeToFirstRow moves' nodeMap' left
                else
                    failwith "no path"

    let moveEmptyNodeBeforeGoal (nodeMap: Map<int * int, Node>) emptyNode goalNode =
        let rec moveRight moves (nodeMap: Map<int * int, Node>) (current: int * int) (goal: int * int) =
            if current = goal then
                (moves, nodeMap)
            else
                let nextP = (fst current + 1, snd current)
                let nextNode = nodeMap.[nextP]
                if canMove nextNode nodeMap.[current] then
                    let nodeMap' = move nodeMap nextP current
                    moveRight (moves + 1) nodeMap' nextP goal
                else
                    failwith "no path"
        moveRight 0 nodeMap emptyNode (goalNode.X - 1, goalNode.Y)

    let moveGoal (nodeMap: Map<int * int, Node>) (goalNodePos: int * int) =
        let rec helper (moves: int) (nodeMap: Map<int * int, Node>) (goalNodePos: int * int) =
            let emptyNode = nodeMap |> Map.findKey (fun _ n -> n.Used = 0)
            let nodeMap' =
                nodeMap
                |> Map.add emptyNode { nodeMap.[emptyNode] with Used = nodeMap.[goalNodePos].Used }
                |> Map.add goalNodePos { nodeMap.[goalNodePos] with Used = 0 }
            let moves' = moves + 1
            let goalNode' = (fst goalNodePos - 1, snd goalNodePos)
            if goalNode' = (0, 0) then
                moves'
            else
                let emptyNode' = (fst goalNode' - 1, snd goalNodePos)
                let nodeMap'' =
                    nodeMap'
                    |> Map.add emptyNode { nodeMap'.[emptyNode'] with Used = 0 }
                let moves'' = moves' + 4
                helper moves'' nodeMap'' goalNode'
        helper 0 nodeMap goalNodePos

    let day22Part2 () =
        let nodes =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.skip 2
            |> Array.map parse
        let nodeMap =
            nodes
            |> Array.map (fun n -> ((n.X, n.Y), n))
            |> Map.ofArray
        let emptyNode = nodeMap |> Map.findKey (fun _ n -> n.Used = 0)
        let maxX =
            nodes
            |> Array.maxBy (fun n -> n.X)
            |> fun n -> n.X
        let goalNode =
            nodes
            |> Array.find (fun n -> n.X = maxX && n.Y = 0)
        let (moves, nodeMap') = moveEmptyNodeToFirstRow 0 nodeMap emptyNode
        let emptyNode' = nodeMap' |> Map.findKey (fun _ n -> n.Used = 0)
        let (moves', nodeMap'') = moveEmptyNodeBeforeGoal nodeMap' emptyNode' goalNode
        let moves'' = moveGoal nodeMap'' (goalNode.X, goalNode.Y)
        moves + moves' + moves''
