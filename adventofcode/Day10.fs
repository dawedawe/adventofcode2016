namespace Adventofcode2016

module Day10 =

    [<Literal>]
    let InputFile = "Day10Input.txt"

    type BotOrOutput =
        | Bot
        | Output

    type ChipCarrier =
        { Id: int * BotOrOutput
          Chip1: int option
          Chip2: int option }

    module ChipCarrier =

        let create id chip =
            { Id = id
              Chip1 = Some chip
              Chip2 = None }

        let (|BotWithNoChip|BotWithOneChip|BotWithTwoChips|EmptyOutput|FilledOutput|) chipCarrier =
            match chipCarrier with
            | { Id = (_, Output)
                Chip1 = None
                Chip2 = None } -> EmptyOutput chipCarrier
            | { Id = (_, Output)
                Chip1 = Some _
                Chip2 = None } -> FilledOutput chipCarrier
            | { Id = (_, Bot)
                Chip1 = None
                Chip2 = None } -> BotWithNoChip chipCarrier
            | { Id = (_, Bot)
                Chip1 = Some _
                Chip2 = None } -> BotWithOneChip chipCarrier
            | { Id = (_, Bot)
                Chip1 = Some _
                Chip2 = Some _ } -> BotWithTwoChips chipCarrier
            | _ -> failwith "illegal ChipCarrier state"

        let addChip chipCarrier chip =
            match chipCarrier with
            | EmptyOutput o -> { o with Chip1 = Some chip }
            | FilledOutput o -> failwith (sprintf "trying to add to filled output %d" (fst o.Id))
            | BotWithNoChip bot -> { bot with Chip1 = Some chip }
            | BotWithOneChip bot ->
                { bot with
                      Chip1 = Some(min bot.Chip1.Value chip)
                      Chip2 = Some(max bot.Chip1.Value chip) }
            | BotWithTwoChips _ -> failwith "bot already fully chipped"

        let hasChips bot chip1 chip2 =
            match bot with
            | { Id = _
                Chip1 = Some c1
                Chip2 = Some c2 } when c1 = chip1 && c2 = chip2 -> true
            | _ -> false

        let isFullyChipped bot =
            match bot with
            | BotWithTwoChips _ -> true
            | _ -> false

    let parseInitLine (s: string) =
        let words = s.Split(' ')
        int words.[1], int words.[5]

    let parseGiveLine (s: string) =
        let words = s.Split(' ')

        let lowTargetType =
            if words.[5] = "bot" then
                Bot
            else
                Output

        let highTargetType =
            if words.[10] = "bot" then
                Bot
            else
                Output

        int words.[1], lowTargetType, int words.[6], highTargetType, int words.[11]

    let init lines =
        let (initLines, ruleLines) =
            lines
            |> Array.partition (fun (s: string) -> s.StartsWith("value "))

        let helper (bots: Map<(int * BotOrOutput), ChipCarrier>) initLine =
            let (v, botNr) = parseInitLine initLine
            let entry = Map.tryFind (botNr, Bot) bots

            let bot =
                match entry with
                | Some b -> ChipCarrier.addChip b v
                | None -> ChipCarrier.create (botNr, Bot) v

            Map.add (botNr, Bot) bot bots

        let initState = Array.fold helper Map.empty initLines
        (initState, ruleLines)

    let passChip botNr chip t (chipCarriers: Map<int * BotOrOutput, ChipCarrier>) =
        let entry = Map.tryFind (botNr, t) chipCarriers

        let bot =
            match entry with
            | Some b -> ChipCarrier.addChip b chip
            | None -> ChipCarrier.create (botNr, t) chip

        Map.add (botNr, t) bot chipCarriers

    let rec step (lines: string []) (chipCarriers: Map<(int * BotOrOutput), ChipCarrier>) =
        let responsibleBot =
            chipCarriers
            |> Map.tryFindKey (fun _ v -> ChipCarrier.hasChips v 17 61)

        match responsibleBot with
        | Some (nr, _) -> nr
        | None ->
            let rules = lines |> Array.map parseGiveLine

            let bot =
                chipCarriers
                |> Map.filter (fun _ v -> ChipCarrier.isFullyChipped v)
                |> Map.toArray
                |> Array.map snd
                |> Array.head

            let (_, lowType, lowTarg, highType, highTarg) =
                Array.find (fun (b, _, _, _, _) -> b = fst bot.Id) rules

            chipCarriers
            |> passChip lowTarg bot.Chip1.Value lowType
            |> passChip highTarg bot.Chip2.Value highType
            |> Map.add
                bot.Id
                { Id = bot.Id
                  Chip1 = None
                  Chip2 = None }
            |> step lines

    let day10 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let (chipCarriers, ruleLines) = init lines
        step ruleLines chipCarriers

    let rec stepPart2 rules (chipCarriers: Map<int * BotOrOutput, ChipCarrier>) =
        let outputs012 =
            chipCarriers
            |> Map.filter
                (fun k v -> (k = (0, Output) || k = (1, Output) || k = (2, Output)) && v.Chip1.IsSome)

        if outputs012.Count = 3 then
            outputs012
            |> Map.toArray
            |> Array.map (snd >> fun x -> x.Chip1.Value)
            |> Array.fold (*) 1
        else
            let bot =
                chipCarriers
                |> Map.filter (fun _ v -> ChipCarrier.isFullyChipped v)
                |> Map.toArray
                |> Array.map snd
                |> Array.head

            let matchingRule, restOfRules =
                Array.partition (fun (b, _, _, _, _) -> b = (fst bot.Id)) rules

            let (_, lowType, lowTarg, highType, highTarg) = Array.head matchingRule

            chipCarriers
            |> passChip lowTarg bot.Chip1.Value lowType
            |> passChip highTarg bot.Chip2.Value highType
            |> Map.remove bot.Id
            |> stepPart2 restOfRules

    let day10Part2 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let (chipCarriers, ruleLines) = init lines
        let rules = ruleLines |> Array.map parseGiveLine
        stepPart2 rules chipCarriers
