namespace Adventofcode2016

module Day22 =

    [<Literal>]
    let InputFile = "Day22Input.txt"

    type Node =
        { X: int
          Y: int
          Size: int
          Used: int
          Avail: int }

    let parse (s: string) =
        let r =
            System.Text.RegularExpressions.Regex(@"/dev/grid/node-x(\d+)-y(\d+)(\s*)(\d+)T(\s*)(\d+)T(\s*)(\d+)T")
        let m = r.Match(s)
        { X = int m.Groups.[1].Value
          Y = int m.Groups.[2].Value
          Size = int m.Groups.[4].Value
          Used = int m.Groups.[6].Value
          Avail = int m.Groups.[8].Value }

    let isPair n1 n2 =
        n1.Used > 0 && n1 <> n2 && n1.Used <= n2.Avail

    let getPairs node nodes =
        nodes |> Array.filter (fun n -> isPair node n)

    let day22 () =
        let nodes =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.skip 2
            |> Array.map parse
        nodes
        |> Array.collect (fun n -> getPairs n nodes)
        |> Array.length
