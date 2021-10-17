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

    let rec turn input currentId (circle: Map<int, int>) =
        let posToRob = getIdToTheLeft currentId circle

        let circle' =
            circle
            |> Map.add currentId (circle.[currentId] + circle.[posToRob])
            |> Map.add posToRob 0

        if circle'.[currentId] = input then
            currentId + 1
        else
            let currentId' = getIdToTheLeft currentId circle'
            turn input currentId' circle'

    let day19 () =

        let mutable pos = 3

        for i in 6 .. Input do
            let pos' = pos + 2
            pos <- if pos' > i then pos' % i else pos'

        pos
