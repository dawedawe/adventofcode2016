namespace Adventofcode2016

module Day05 =

    let hash (md5: System.Security.Cryptography.MD5) (input: string) =
        let bytes = System.Text.Encoding.ASCII.GetBytes(input)
        let hash = md5.ComputeHash(bytes)
        let output = sprintf "%02X%02X%02X%02X" hash.[0] hash.[1] hash.[2] hash.[3]
        output

    let day05() =
        let md5 = System.Security.Cryptography.MD5.Create()
        let mutable i = 0
        let mutable pw = ""
        while (pw.Length <> 8) do
            let h = hash md5 ("abbhdwsy" + string i)
            if (h.StartsWith("00000")) then pw <- pw + string h.[5]
            i <- i + 1
        pw
