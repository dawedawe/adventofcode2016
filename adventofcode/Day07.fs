namespace Adventofcode2016

module Day07 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day07Input.txt"

    let getIPs =
        System.IO.File.ReadAllLines

    let getHypers (s: string) =
        let rec helper (ip: string) (hypers: string list) =
            let openBracket = ip.IndexOf("[")
            if (openBracket < 0) then
                hypers
            else
                let closeBracket = ip.IndexOf("]")
                let hyper = ip.Substring(openBracket + 1, closeBracket - openBracket - 1)
                let hypers' = hypers @ [ hyper ]
                let ip' = ip.Substring(closeBracket + 1)
                helper ip' hypers'
        helper s []

    let getNonHypers (s: string) =
        let rec helper (ip: string) (nonHypers: string list) =
            let openBracket = ip.IndexOf("[")
            if (openBracket > 0) then
                let nonHyper = ip.Substring(0, openBracket)
                let nonHypers' = nonHypers @ [ nonHyper ]
                let closeBracket = ip.IndexOf("]")
                let ip' = ip.Substring(closeBracket + 1)
                helper ip' nonHypers'
            else
                let nonHypers' = nonHypers @ [ ip ]
                nonHypers'
        helper s []

    let containsABBA (str: string) =
        let rec helper (c: char list) =
            match c with
            | [] -> false
            | a1 :: b1 :: b2 :: a2 :: _ when a1 = a2 && b1 = b2 && a1 <> b1 -> true
            | _ -> helper c.[1..]
        str.ToCharArray()
        |> List.ofArray
        |> helper

    let supportsTLS (s: string) =
        let hypers = getHypers s
        let nonHypers = getNonHypers s

        let hypersWithAbba =
            hypers
            |> List.sumBy (fun s ->
                if (containsABBA s) then 1 else 0)
            |> fun c -> c > 0

        let nonHypersWithAbba =
            nonHypers
            |> List.sumBy (fun s ->
                if (containsABBA s) then 1 else 0)
            |> fun c -> c > 0

        nonHypersWithAbba && not hypersWithAbba

    let day07() =
        let ips = getIPs InputFile
        let withTLS = Array.filter supportsTLS ips
        Array.length withTLS

    let getABAs (s: string) =
        let rec helper (cs: char list) abas =
            match cs with
            | [] -> abas
            | a1 :: b :: a2 :: _ when a1 = a2 && a1 <> b ->
                let abas' = abas @ [ string a1 + string b + string a2 ]
                helper cs.[1..] abas'
            | _ -> helper cs.[1..] abas
        s.ToCharArray()
        |> List.ofArray
        |> fun s -> helper s []

    let transformABAsToBABs (abas: string list) =
        let helper (s: string) =
            string s.[1] + string s.[0] + string s.[1]
        List.map helper abas

    let supportsSSL (s: string) =
        let hypers = getHypers s
        let superNetSeqs = getNonHypers s

        let superNetSeqsABAs =
            superNetSeqs
            |> List.map getABAs
            |> List.collect id

        let neededBABs = transformABAsToBABs superNetSeqsABAs

        let anyCorrespondingBABs =
            neededBABs
            |> List.map (fun bab -> hypers |> List.map (fun h -> h.Contains bab))
            |> List.collect id
            |> List.filter id
            |> fun l -> l.Length > 0

        anyCorrespondingBABs

    let day07Part2() =
        let ips = getIPs InputFile
        let withSSL = Array.filter supportsSSL ips
        Array.length withSSL
