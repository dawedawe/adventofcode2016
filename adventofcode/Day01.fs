namespace Adventofcode2016

module Day01 =

    [<Literal>]
    let InputFile = "Day01Input.txt"

    let parseInput (input: string) =
        input.Split([| ',' |]) |> Array.map (fun s -> s.Trim())

    type Direction =
        | North
        | East
        | South
        | West

    type Move =
        | Left of int
        | Right of int

    let (|LeftMove|RightMove|) (s: string) =
        match s.[0] with
        | 'L' -> LeftMove(System.Int32.Parse s.[1..])
        | 'R' -> RightMove(System.Int32.Parse s.[1..])
        | _ -> failwith "bad input"

    let parseMove s =
        match s with
        | LeftMove x -> Left x
        | RightMove x -> Right x

    let getInput path =
        path
        |> System.IO.File.ReadAllText
        |> parseInput
        |> Array.map parseMove

    let goNorth (x, y) d = (x, y + d)
    let goEast (x, y) d = (x + d, y)
    let goSouth (x, y) d = (x, y - d)
    let goWest (x, y) d = (x - d, y)

    let makeMove move pos dir =
        match (move, dir) with
        | (Left d, North) -> (goWest pos d, West)
        | (Left d, East) -> (goNorth pos d, North)
        | (Left d, South) -> (goEast pos d, East)
        | (Left d, West) -> (goSouth pos d, South)
        | (Right d, North) -> (goEast pos d, East)
        | (Right d, East) -> (goSouth pos d, South)
        | (Right d, South) -> (goWest pos d, West)
        | (Right d, West) -> (goNorth pos d, North)

    let follow m =
        let rec helper currentDir currentPos moves =
            match moves with
            | [] -> currentPos
            | m :: r ->
                let (pos', dir') = makeMove m currentPos currentDir
                helper dir' pos' r
        helper North (0, 0) m

    let day01() =
        let input = getInput InputFile |> List.ofArray
        let (x, y) = follow input
        let distance = abs x + abs y
        distance

    let getPositions (x, y) (x', y') =
        if x <> x' then
            let step =
                if x < x' then 1 else -1
            seq {
                for a in x .. step .. x' do
                    yield (a, y)
            }
        else
            let step =
                if y < y' then 1 else -1
            seq {
                for a in y .. step .. y' do
                    yield (x, a)
            }

    let followTillRepeat m =
        let rec helper path currentDir currentPos moves =
            match moves with
            | [] -> currentPos
            | m :: r ->
                let (pos', dir') = makeMove m currentPos currentDir

                let wayPoints =
                    getPositions currentPos pos'
                    |> Set.ofSeq
                    |> Set.remove currentPos

                let interSect = Set.intersect path wayPoints
                if (Set.isEmpty interSect) then
                    let path' = Set.union path wayPoints
                    helper path' dir' pos' r
                else
                    interSect
                    |> Set.toArray
                    |> fun a -> a.[0]


        let start = (0, 0)
        helper Set.empty North start m

    let day01Part2() =
        let input = getInput InputFile |> List.ofArray
        let (x, y) = followTillRepeat input
        let distance = abs x + abs y
        distance
