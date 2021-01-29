namespace Adventofcode2016

module Day10 =

    [<Literal>]
    let InputFile = "Day10Input.txt"

    type Bot =
        { Nr: int
          Chip1: int option
          Chip2: int option }

    module Bot =

        let create nr chip =
            { Nr = nr
              Chip1 = Some chip
              Chip2 = None }

        let (|BotWithNoChip|BotWithOneChip|BotWithTwoChips|) bot =
            match bot with
            | { Nr = _; Chip1 = None; Chip2 = None } -> BotWithNoChip bot
            | { Nr = _; Chip1 = Some _; Chip2 = None } -> BotWithOneChip bot
            | { Nr = _
                Chip1 = Some _
                Chip2 = Some _ } -> BotWithTwoChips bot
            | _ -> failwith "illegal bot state"

        let addChip bot chip =
            match bot with
            | BotWithNoChip bot -> { bot with Chip1 = Some chip }
            | BotWithOneChip bot ->
                { bot with
                      Chip1 = Some(min bot.Chip1.Value chip)
                      Chip2 = Some(max bot.Chip1.Value chip) }
            | BotWithTwoChips _ -> failwith "bot already fully chipped"

        let hasChips bot chip1 chip2 =
            match bot with
            | { Nr = _
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
        int words.[1], int words.[6], int words.[11]

    let init lines =
        let (initLines, ruleLines) =
            lines
            |> Array.partition (fun (s: string) -> s.StartsWith("value "))

        let helper (bots: Map<int, Bot>) initLine =
            let (v, botNr) = parseInitLine initLine
            let entry = Map.tryFind botNr bots

            let bot =
                match entry with
                | Some b -> Bot.addChip b v
                | None -> Bot.create botNr v

            Map.add botNr bot bots

        let initState = Array.fold helper Map.empty initLines
        (initState, ruleLines)

    let passChip botNr chip (bots: Map<int, Bot>) =
        let entry = Map.tryFind botNr bots

        let bot =
            match entry with
            | Some b -> Bot.addChip b chip
            | None -> Bot.create botNr chip

        Map.add botNr bot bots

    let rec step (lines: string []) (bots: Map<int, Bot>) =
        let responsibleBot =
            bots
            |> Map.tryFindKey (fun _ v -> Bot.hasChips v 17 61)

        match responsibleBot with
        | Some nr -> nr
        | None ->
            let rules = lines |> Array.map parseGiveLine

            let bot =
                bots
                |> Map.filter (fun _ v -> Bot.isFullyChipped v)
                |> Map.toArray
                |> Array.map snd
                |> Array.head

            let (_, lowTarg, highTarg) =
                Array.find (fun (b, _, _) -> b = bot.Nr) rules

            bots
            |> passChip lowTarg bot.Chip1.Value
            |> passChip highTarg bot.Chip2.Value
            |> Map.add
                bot.Nr
                { Nr = bot.Nr
                  Chip1 = None
                  Chip2 = None }
            |> step lines

    let day10 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let (bots, ruleLines) = init lines
        step ruleLines bots
