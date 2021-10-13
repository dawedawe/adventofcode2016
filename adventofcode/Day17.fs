namespace Adventofcode2016

module Day17 =

    let Input = "mmsxrhfx"

    let goal = (3, 3)

    type Node = { Path: string; Pos: int * int }

    let md5 =
        System.Security.Cryptography.MD5.Create()

    let hash input path =
        let data = sprintf "%s%s" input path

        let hash =
            md5.ComputeHash(System.Text.Encoding.ASCII.GetBytes(data))

        sprintf "%02x%02x" hash.[0] hash.[1]

    let isDoorOpen =
        function
        | 'b'
        | 'c'
        | 'd'
        | 'e'
        | 'f' -> true
        | _ -> false

    let getDoorStates (h: string) =
        let up = isDoorOpen h.[0]
        let down = isDoorOpen h.[1]
        let left = isDoorOpen h.[2]
        let right = isDoorOpen h.[3]
        (up, down, left, right)

    let getValidDirections (x, y) (u, d, l, r) =
        seq {
            if y > 0 && u then yield "U"
            if y < 3 && d then yield "D"
            if x > 0 && l then yield "L"
            if x < 3 && r then yield "R"
        }

    let move (x, y) =
        function
        | "U" -> (x, y - 1)
        | "D" -> (x, y + 1)
        | "L" -> (x - 1, y)
        | "R" -> (x + 1, y)
        | _ -> failwith "bad direction"

    let bfs whilePred conditionF =
        let queue = System.Collections.Generic.Queue<Node>()
        let mutable pathFound = None
        queue.Enqueue({ Path = ""; Pos = (0, 0) })

        while queue.Count > 0 && whilePred pathFound do
            let node = queue.Dequeue()

            if node.Pos <> goal then
                let dirs =
                    hash Input node.Path
                    |> getDoorStates
                    |> getValidDirections node.Pos

                for d in dirs do
                    let newPos = move node.Pos d
                    let newPath = node.Path + d
                    let newNode = { Path = newPath; Pos = newPos }
                    queue.Enqueue(newNode)

            else if conditionF node pathFound then
                pathFound <- Some node.Path

        if Option.isSome pathFound then
            pathFound.Value
        else
            failwith "no path found"

    let day17 () = bfs Option.isNone (fun _ _ -> true)

    let part2F node (pathFound: string option) =
        let length =
            pathFound
            |> Option.map String.length
            |> Option.defaultValue 0

        node.Path.Length > length

    let day17Part2 () =
        bfs (fun _ -> true) part2F |> String.length
