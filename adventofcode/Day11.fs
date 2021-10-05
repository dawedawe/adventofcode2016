namespace Adventofcode2016

open System.Collections.Generic
open System.Linq

module Day11 =

    [<Literal>]
    let InputFile = "Day11Input.txt"

    type Chip =
        { Name: string }
        static member Create n = { Name = n }

    type Generator =
        { Name: string }
        static member Create n = { Name = n }


    type Floor =
        { Chips: Chip Set
          Generators: Generator Set }
        static member Create level chips generators =
            { Chips = chips
              Generators = generators }

        member __.IsEmpty =
            Set.isEmpty __.Chips && Set.isEmpty __.Generators

    type State =
        { ElevatorLevel: int
          Floors: Floor [] }

    type Path = { States: State list }

    let printState (s: State) =
        for i in 4 .. -1 .. 1 do
            let e =
                if s.ElevatorLevel = (i - 1) then
                    "E"
                else
                    "."

            let gens =
                s.Floors.[i - 1].Generators
                |> Set.map (fun g -> g.Name)

            let chips =
                s.Floors.[i - 1].Chips
                |> Set.map (fun c -> c.Name)

            printfn "F%d %s G: %A M: %A" i e gens chips

        printfn ""

    let isGoalState state =
        state.Floors.[0].IsEmpty
        && state.Floors.[1].IsEmpty
        && state.Floors.[2].IsEmpty

    let parseLevel =
        function
        | "first" -> 0
        | "second" -> 1
        | "third" -> 2
        | "fourth" -> 3
        | _ -> failwith "unsupported level"

    let parseGenerators (s: string) =
        let r =
            System.Text.RegularExpressions.Regex(@"a ((\w+)) generator")

        let matches = r.Matches(s)

        [ for m in matches -> Generator.Create m.Groups.[1].Value ]
        |> Set.ofList

    let parseChips (s: string) =
        let r =
            System.Text.RegularExpressions.Regex(@"a ((\w+))-compatible microchip")

        let matches = r.Matches(s)

        [ for m in matches -> Chip.Create m.Groups.[1].Value ]
        |> Set.ofList

    let parseLine (s: string) =
        let words = s.Split(' ')
        let level = parseLevel words.[1]
        let generators = parseGenerators s
        let chips = parseChips s
        Floor.Create level chips generators

    let isLegalRoom floor =
        let allChipsConnected =
            seq {
                for c in floor.Chips do
                    let isConnected =
                        floor.Generators.Contains(Generator.Create c.Name)

                    let chipIsOkay = isConnected
                    yield chipIsOkay
            }
            |> Seq.fold (&&) true

        floor.Chips.IsEmpty
        || floor.Generators.IsEmpty
        || allChipsConnected

    let chipGenConfig (state: State) =
        seq {
            for chipLevel in 0 .. 3 do
                for chip in state.Floors.[chipLevel].Chips do
                    for genLevel in 0 .. 3 do
                        for gen in state.Floors.[genLevel].Generators do
                            if chip.Name = gen.Name then
                                yield (chipLevel, genLevel)
        }
        |> Seq.toList

    let equivalent (chipGenConf1: (int * int) list) (chipGenConf2: (int * int) list) =

        let conf1Groups = chipGenConf1 |> List.groupBy id
        let conf2Groups = chipGenConf2 |> List.groupBy id

        [ for (g1Key, g1Values) in conf1Groups do
              conf2Groups
              |> List.tryFind (fun (k, v) -> k = g1Key && v.Length = g1Values.Length)
              |> Option.isSome ]
        |> List.reduce (&&)
        && conf1Groups.Length = conf2Groups.Length

    let getSubSets (s: Set<'a>) =
        seq {
            for c in s do
                let rest = Set.remove c s

                for r in rest do
                    yield [ c; r ] |> Set.ofList

                yield Set.singleton c
        }
        |> Set.ofSeq

    let getChipGenPairs (chips: Chip Set) (generators: Generator Set) =
        seq {
            for c in chips do
                let genOfC = Generator.Create c.Name

                if Set.contains genOfC generators then
                    yield Set.singleton c, Set.singleton genOfC
        }

    let h (x: State) =
        (x.Floors.[0].Chips.Count
         + x.Floors.[0].Generators.Count)
        * 4
        + (x.Floors.[1].Chips.Count
           + x.Floors.[1].Generators.Count)
          * 3
        + (x.Floors.[2].Chips.Count
           + x.Floors.[2].Generators.Count)
        + (x.Floors.[3].Chips.Count
           + x.Floors.[3].Generators.Count)
          * (-5)

    let getNextPossibleStates (state: State) =
        let currentLevel = state.ElevatorLevel
        let lowerLevel = currentLevel - 1
        let upperLevel = currentLevel + 1
        let lowerLevelOk = lowerLevel >= 0
        let upperLevelOk = upperLevel < 4

        let possibleChipSets =
            getSubSets state.Floors.[currentLevel].Chips

        let possibleGeneratorSets =
            getSubSets state.Floors.[currentLevel].Generators

        let matchingChipGenPairs =
            getChipGenPairs state.Floors.[currentLevel].Chips state.Floors.[currentLevel].Generators

        let states =
            seq {
                for (chip, gen) in matchingChipGenPairs do
                    let reducedChips =
                        Set.difference state.Floors.[currentLevel].Chips chip

                    let reducedGens =
                        Set.difference state.Floors.[currentLevel].Generators gen

                    if upperLevelOk then
                        let floors = Array.copy state.Floors

                        floors.[currentLevel] <-
                            { Generators = reducedGens
                              Chips = reducedChips }

                        floors.[upperLevel] <-
                            { Generators = Set.union gen floors.[upperLevel].Generators
                              Chips = Set.union chip floors.[upperLevel].Chips }

                        let stateAfterUpMove =
                            { ElevatorLevel = upperLevel
                              Floors = floors }

                        if (isLegalRoom floors.[currentLevel])
                           && (isLegalRoom floors.[upperLevel]) then
                            yield stateAfterUpMove

                if upperLevelOk then
                    for possibleChipSet in possibleChipSets do
                        let reducedChips =
                            Set.difference state.Floors.[currentLevel].Chips possibleChipSet

                        let floors = Array.copy state.Floors

                        floors.[currentLevel] <-
                            { Generators = floors.[currentLevel].Generators
                              Chips = reducedChips }

                        floors.[upperLevel] <-
                            { Generators = floors.[upperLevel].Generators
                              Chips = Set.union possibleChipSet floors.[upperLevel].Chips }

                        let stateAfterUpMove =
                            { ElevatorLevel = upperLevel
                              Floors = floors }

                        if (isLegalRoom floors.[currentLevel])
                           && (isLegalRoom floors.[upperLevel]) then
                            yield stateAfterUpMove

                if upperLevelOk then
                    for possibleGenSet in possibleGeneratorSets do
                        let reducedGens =
                            Set.difference state.Floors.[currentLevel].Generators possibleGenSet

                        let floors = Array.copy state.Floors

                        floors.[currentLevel] <-
                            { Chips = floors.[currentLevel].Chips
                              Generators = reducedGens }

                        floors.[upperLevel] <-
                            { Chips = floors.[upperLevel].Chips
                              Generators = Set.union possibleGenSet floors.[upperLevel].Generators }

                        let stateAfterUpMove =
                            { ElevatorLevel = upperLevel
                              Floors = floors }

                        if (isLegalRoom floors.[currentLevel])
                           && (isLegalRoom floors.[upperLevel]) then
                            yield stateAfterUpMove

                if lowerLevelOk then
                    for possibleGenSet in possibleGeneratorSets do
                        let reducedGens =
                            Set.difference state.Floors.[currentLevel].Generators possibleGenSet

                        let floors = Array.copy state.Floors

                        floors.[currentLevel] <-
                            { Chips = floors.[currentLevel].Chips
                              Generators = reducedGens }

                        floors.[lowerLevel] <-
                            { Chips = floors.[lowerLevel].Chips
                              Generators = Set.union possibleGenSet floors.[lowerLevel].Generators }

                        let stateAfterDownMove =
                            { ElevatorLevel = lowerLevel
                              Floors = floors }

                        if (isLegalRoom floors.[currentLevel])
                           && (isLegalRoom floors.[lowerLevel]) then
                            yield stateAfterDownMove
            }

        states

    let containtsEquivalent s (allStatesEver: inref<State list>) =
        let sConfig = chipGenConfig s

        List.tryFind (fun x -> equivalent (chipGenConfig x) sConfig) allStatesEver
        |> Option.isSome

    let addAt (openList: SortedList<int, Path>) (f: int) (successorPath: Path) =
        if openList.ContainsKey f then
            let prior =
                openList.TakeWhile(fun x -> x.Key < f).ToList()

            let upper =
                openList
                    .Skip(prior.Count)
                    .Select(fun p -> KeyValuePair(p.Key + 1, p.Value))
                    .ToList()

            openList.Clear()

            for p in prior do
                openList.Add(p.Key, p.Value)

            openList.Add(f, successorPath)

            for p in upper do
                openList.Add(p.Key, p.Value)
        else
            openList.Add(f, successorPath)

    let aStar (state: State) =
        let initialPath = { States = [ state ] }
        let openList = SortedList<int, Path>()
        let mutable closedList = Set.empty<Path>
        openList.Add(0, initialPath)
        let mutable goalPath = None

        while openList.Count > 0 && Option.isNone goalPath do
            let minKey = openList.Keys.Min()
            let currentNode = openList.Item(minKey)
            openList.Remove(minKey) |> ignore
            let currentNodeState = List.last currentNode.States

            if isGoalState currentNodeState then
                goalPath <- Some currentNode
            else
                closedList <- Set.add currentNode closedList

                let successors = getNextPossibleStates currentNodeState

                for successor in successors do
                    let successorPath =
                        { States = List.append currentNode.States [ successor ] }

                    if not (closedList.Contains(successorPath)) then
                        let equiInPast =
                            containtsEquivalent successor &currentNode.States

                        let tentativeg = (currentNode.States.Length + 1)

                        if equiInPast then
                            ()
                        else
                            let f = tentativeg + h (successor)

                            if (openList.ContainsValue(successorPath)) then
                                let idx = openList.IndexOfValue(successorPath)
                                openList.RemoveAt(idx)
                                openList.Add(f, successorPath)
                            else
                                addAt openList f successorPath

        goalPath

    let day11 () =
        let floors =
            System.IO.File.ReadAllLines InputFile
            |> Array.map parseLine

        let state = { ElevatorLevel = 0; Floors = floors }

        let goalPath = aStar state

        match goalPath with
        | Some p -> p.States.Length - 1
        | None -> -1

    let day11Part2 () =
        let floors =
            System.IO.File.ReadAllLines InputFile
            |> Array.map parseLine

        let part2Chips =
            Set.ofList [ Chip.Create "elerium"
                         Chip.Create "dilithium" ]

        let part2Generators =
            Set.ofList [ Generator.Create "elerium"
                         Generator.Create "dilithium" ]

        floors.[0] <-
            { Chips = Set.union floors.[0].Chips part2Chips
              Generators = Set.union floors.[0].Generators part2Generators }

        let state = { ElevatorLevel = 0; Floors = floors }

        let goalPath = aStar state

        match goalPath with
        | Some p -> p.States.Length - 1
        | None -> -1
