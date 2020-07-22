namespace Adventofcode2016

module Day04 =

    [<Literal>]
    let InputFile = "Day04Input.txt"

    let getInput path =
        path |> System.IO.File.ReadAllLines

    let getSectorId (s: string) =
        let endIndex = s.IndexOf('[') - 1
        let startIndex = endIndex - 2
        let claimedCheckSum = s.[startIndex..endIndex]
        int claimedCheckSum

    let calcChecksum (s: string) =
        let letters =
            s.Replace("-", "")
            |> fun s -> s.ToCharArray()
            |> Array.takeWhile System.Char.IsLetter

        let sorted =
            letters
            |> Array.countBy id
            |> Array.sortByDescending (fun (_, i) -> i)
            |> Array.groupBy (fun (_, i) -> i)

        seq {
            for group in sorted do
                let (_, g) = group
                let sortedG = g |> Array.sortBy (fun (c, _) -> c)
                yield sortedG
        }
        |> Seq.collect id
        |> Seq.take 5
        |> List.ofSeq
        |> List.map (fun (c, _) -> c)
        |> List.fold (fun s c -> s + string c) ""

    let isRealRoom (s: string) =
        let start = s.IndexOf('[')
        let claimedCheckSum = s.[start + 1..start + 5]
        let realCheckSum = calcChecksum s
        realCheckSum = claimedCheckSum

    let day04() =
        getInput InputFile
        |> Array.filter isRealRoom
        |> Array.sumBy getSectorId
