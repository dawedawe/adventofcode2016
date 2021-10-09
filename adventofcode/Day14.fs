namespace Adventofcode2016

module Day14 =

    [<Literal>]
    let Input = "jlmsuwbz"

    type Candidate = { Index: int; Triple: string }

    let md5 =
        System.Security.Cryptography.MD5.Create()

    let hash salt index =
        let data = sprintf "%s%d" salt index

        let hash =
            md5.ComputeHash(System.Text.Encoding.ASCII.GetBytes(data))

        [| for b in hash -> sprintf "%02x" b |]
        |> String.concat ("")

    let (|Triple|_|) (h: string) =
        [ 2 .. 31 ]
        |> List.tryPick
            (fun i ->
                if h.[i - 2] = h.[i - 1] && h.[i - 1] = h.[i] then
                    Some(h.Substring(i - 2, 3))
                else
                    None)

    let (|Fiver|_|) (h: string) =
        [ 4 .. 31 ]
        |> List.tryPick
            (fun i ->
                if h.[i - 4] = h.[i - 3]
                   && h.[i - 3] = h.[i - 2]
                   && h.[i - 2] = h.[i - 1]
                   && h.[i - 1] = h.[i] then
                    Some(h.Substring(i - 4, 5))
                else
                    None)

    let rec iteration (index: int) candidates keyIndexes =
        let indexWindowStart = index - 1000

        let validCandidates =
            List.filter (fun c -> c.Index >= indexWindowStart) candidates

        let h = hash Input index

        let candidates', keys' =
            match h with
            | Fiver f ->
                let triple = f.Substring(0, 3)
                let candidateToAdd = [ { Index = index; Triple = triple } ]

                let matchingCandidates, restCandidates =
                    validCandidates
                    |> List.partition (fun c -> c.Triple = triple)

                let matchedIndexes =
                    matchingCandidates |> List.map (fun c -> c.Index)

                (restCandidates @ candidateToAdd, keyIndexes @ matchedIndexes)
            | Triple t ->
                let extendedCandidates =
                    List.append validCandidates [ { Index = index; Triple = t } ]

                (extendedCandidates, keyIndexes)
            | _ -> (validCandidates, keyIndexes)

        if keyIndexes.Length >= 64 then
            keyIndexes.[63]
        else
            iteration (index + 1) candidates' keys'

    let day14 () = iteration 0 List.empty List.empty
