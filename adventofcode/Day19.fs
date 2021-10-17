namespace Adventofcode2016

module Day19 =

    [<Literal>]
    let Input = 3004953

    let getIdToTheLeft currentId (circle: Map<int, int>) =
        let upperKey =
            circle
            |> Map.tryFindKey (fun id c -> id > currentId && c > 0)

        match upperKey with
        | Some key -> key
        | None ->
            circle
            |> Map.findKey (fun id c -> id < currentId && c > 0)

    let getIdAcross currentId (circle: Map<int, int>) =
        let stillInGameCircle = circle |> Map.filter (fun _ c -> c > 0)
        let stillInGameCount = stillInGameCircle |> Map.count
        let distance = stillInGameCount / 2
        let keys = stillInGameCircle |> Map.toList |> List.map fst
        let indexOfCurrentId = keys |> List.findIndex (fun id -> id = currentId)
        let idAcross = (indexOfCurrentId + distance) % stillInGameCount
        keys.[idAcross]

    let rec turn getNextId getIdToRob input currentId (circle: Map<int, int>) =
        let posToRob = getIdToRob currentId circle

        let circle' =
            circle
            |> Map.add currentId (circle.[currentId] + circle.[posToRob])
            |> Map.add posToRob 0

        if circle'.[currentId] = input then
            currentId
        else
            let currentId' = getNextId currentId circle'
            turn getNextId getIdToRob input currentId' circle'

    let day19 () =

        let mutable pos = 3

        for i in 6 .. Input do
            let pos' = pos + 2
            pos <- if pos' > i then pos' % i else pos'

        pos

    let day19Part2 () =

        let mutable pos = 0
        let mutable increments = 1
        let mutable i = 2
        let mutable result = 0
        while i <= Input do

            for j in 1 .. increments do
                let pos' = pos + 1
                pos <- if pos' >= i then 1 else pos'
                if i = Input then result <- pos
                i <- i + 1

            for j in 1 .. increments do
                let pos' = pos + 2
                pos <- if pos' > i then pos' % i else pos'
                if i = Input then result <- pos
                i <- i + 1

            increments <- increments * 3

        result