namespace Adventofcode2016

module Day16 =

    let Input = "01000100010010111"

    let comp c = if c = '1' then '0' else '1'

    let rec expand bits length =
        let a = bits
        let b = a
        let b' = Array.rev b
        let b'' = b' |> Array.map comp
        let r = Array.concat [ a; [| '0' |]; b'' ]

        if r.Length >= length then
            Array.take length r
        else
            expand r length

    let rec checksum bits =
        let reduction =
            [| for i in 0 .. 2 .. (Array.length bits - 2) do
                   if bits.[i] = bits.[i + 1] then
                       '1'
                   else
                       '0' |]

        if reduction.Length % 2 = 0 then
            checksum reduction
        else
            System.String(reduction)

    let day16 () =
        expand (Input.ToCharArray()) 272 |> checksum

    let day16Part2 () =
        expand (Input.ToCharArray()) 35651584 |> checksum
