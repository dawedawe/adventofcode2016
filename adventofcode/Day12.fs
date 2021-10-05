namespace Adventofcode2016

module Day12 =

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type RegOrVal =
        | Reg of char
        | Val of int

    type Inst =
        | Cpy of RegOrVal * char
        | Inc of char
        | Dec of char
        | Jnz of RegOrVal * int

    type Register =
        { Name: char
          Value: int }
        static member Create name = { Name = name; Value = 0 }

    type Computer =
        { Ip: int
          Registers: Map<char, int> }
        static member Create() =
            { Ip = 0
              Registers =
                  [| ('a', 0)
                     ('b', 0)
                     ('c', 0)
                     ('d', 0) |] |> Map.ofArray }
        static member CreatePart2() =
            { Ip = 0
              Registers =
                  [| ('a', 0)
                     ('b', 0)
                     ('c', 1)
                     ('d', 0) |] |> Map.ofArray }


    let parseLine (s: string) =
        let parts = s.Split(' ')
        let part0 = parts.[0]
        let part1 = parts.[1]
        let part2 = if parts.Length = 3 then Some parts.[2] else None
        match part0, part1, part2 with
        | "cpy", x, Some y when System.Char.IsLetter x.[0] -> Inst.Cpy (Reg x.[0], y.[0])
        | "cpy", x, Some y -> Inst.Cpy (Val (int x), y.[0])
        | "inc", x, None -> Inst.Inc x.[0]
        | "dec", x, None -> Inst.Dec x.[0]
        | "jnz", x, Some y when System.Char.IsLetter x.[0] -> Inst.Jnz (Reg x.[0], int y)
        | "jnz", x, Some y -> Inst.Jnz (Val (int x), int y)
        | _ -> failwith "unkown instruction"

    let execute (comp: Computer) (inst: Inst) =
        match inst with
        | Cpy (Reg r1, r2) ->
            let regs = comp.Registers |> Map.add r2 comp.Registers.[r1]
            { comp with Ip = comp.Ip + 1; Registers = regs }
        | Cpy (Val x, r2) ->
            let regs = comp.Registers |> Map.add r2 x
            { comp with Ip = comp.Ip + 1; Registers = regs }
        | Inc r ->
            let regs = comp.Registers |> Map.add r (comp.Registers.[r] + 1)
            { comp with Ip = comp.Ip + 1; Registers = regs }
        | Dec r ->
            let regs = comp.Registers |> Map.add r (comp.Registers.[r] - 1)
            { comp with Ip = comp.Ip + 1; Registers = regs }
        | Jnz (Reg r1, r2) ->
            let ip = if comp.Registers.[r1] <> 0 then comp.Ip + r2 else comp.Ip + 1
            { comp with Ip = ip; }
        | Jnz (Val x, r2) ->
            let ip = if x <> 0 then comp.Ip + r2 else comp.Ip + 1
            { comp with Ip = ip; }

    let rec run (prog: Inst []) (comp: Computer) =
        if comp.Ip >= prog.Length then
            comp
        else
            execute comp prog.[comp.Ip] |> run prog

    let day12 () =
        let prog =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine
        let comp = run prog (Computer.Create())
        comp.Registers.['a']

    let day12Part2 () =
        let prog =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine
        let comp = run prog (Computer.CreatePart2())
        comp.Registers.['a']
