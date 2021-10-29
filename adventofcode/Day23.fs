namespace Adventofcode2016

module Day23 =

    [<Literal>]
    let InputFile = "Day23Input.txt"

    type RegOrVal =
        | Reg of char
        | Val of int

    type Inst =
        | Cpy of RegOrVal * RegOrVal
        | Inc of char
        | Dec of char
        | Jnz of RegOrVal * RegOrVal
        | Tgl of char

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
                [| ('a', 7)
                   ('b', 0)
                   ('c', 0)
                   ('d', 0) |]
                |> Map.ofArray }

    let parseLine (s: string) =
        let parts = s.Split(' ')
        let part0 = parts.[0]
        let part1 = parts.[1]
        let part2 = if parts.Length = 3 then Some parts.[2] else None
        match part0, part1, part2 with
        | "cpy", x, Some y when System.Char.IsLetter x.[0] -> Inst.Cpy(Reg x.[0], Reg y.[0])
        | "cpy", x, Some y -> Inst.Cpy(Val(int x), Reg y.[0])
        | "inc", x, None -> Inst.Inc x.[0]
        | "dec", x, None -> Inst.Dec x.[0]
        | "jnz", x, Some y when
            System.Char.IsLetter x.[0]
            && System.Char.IsLetter y.[0]
            ->
            Inst.Jnz(Reg x.[0], Reg y.[0])
        | "jnz", x, Some y when
            System.Char.IsLetter x.[0]
            && not (System.Char.IsLetter y.[0])
            ->
            Inst.Jnz(Reg x.[0], Val(int y))
        | "jnz", x, Some y when
            not (System.Char.IsLetter x.[0])
            && System.Char.IsLetter y.[0]
            ->
            Inst.Jnz(Val(int x), Reg y.[0])
        | "jnz", x, Some y when
            not (System.Char.IsLetter x.[0])
            && not (System.Char.IsLetter y.[0])
            ->
            Inst.Jnz(Val(int x), Val(int y))
        | "tgl", x, None -> Inst.Tgl x.[0]
        | _ -> failwith "unkown instruction"

    let toggle idx prog =
        if idx < 0 || idx >= Array.length prog then
            prog
        else
            let instr =
                match prog.[idx] with
                | Inc c -> Dec c
                | Dec c -> Inc c
                | Tgl r -> Inc r
                | Jnz (rv1, rv2) -> Cpy(rv1, rv2)
                | Cpy (rv1, rv2) -> Jnz(rv1, rv2)
            prog.[idx] <- instr
            prog

    let execute ((prog, comp): (Inst [] * Computer)) =
        match prog.[comp.Ip] with
        | Cpy (Reg r1, Reg r2) ->
            let regs = comp.Registers |> Map.add r2 comp.Registers.[r1]
            prog,
            { comp with
                Ip = comp.Ip + 1
                Registers = regs }
        | Cpy (Val x, Reg r2) ->
            let regs = comp.Registers |> Map.add r2 x
            prog,
            { comp with
                Ip = comp.Ip + 1
                Registers = regs }
        | Cpy (_, Val _) -> prog, { comp with Ip = comp.Ip + 1 }
        | Inc r ->
            let regs =
                comp.Registers
                |> Map.add r (comp.Registers.[r] + 1)
            prog,
            { comp with
                Ip = comp.Ip + 1
                Registers = regs }
        | Dec r ->
            let regs =
                comp.Registers
                |> Map.add r (comp.Registers.[r] - 1)
            prog,
            { comp with
                Ip = comp.Ip + 1
                Registers = regs }
        | Jnz (Reg r1, Reg r2) ->
            let ip =
                if comp.Registers.[r1] <> 0 then
                    comp.Ip + comp.Registers.[r2]
                else
                    comp.Ip + 1
            prog, { comp with Ip = ip }
        | Jnz (Val x, Reg r2) ->
            let ip =
                if x <> 0 then
                    comp.Ip + comp.Registers.[r2]
                else
                    comp.Ip + 1
            prog, { comp with Ip = ip }
        | Jnz (Reg r1, Val v) ->
            let ip =
                if comp.Registers.[r1] <> 0 then
                    comp.Ip + v
                else
                    comp.Ip + 1
            prog, { comp with Ip = ip }
        | Jnz (Val x, Val v) ->
            let ip = if x <> 0 then comp.Ip + v else comp.Ip + 1
            prog, { comp with Ip = ip }
        | Tgl r ->
            let x = comp.Registers.[r]
            let idx = comp.Ip + x
            let prog' = toggle idx prog
            prog', { comp with Ip = comp.Ip + 1 }

    let rec run ((prog, comp): (Inst [] * Computer)) =
        if comp.Ip >= prog.Length then
            comp
        else
            execute (prog, comp) |> run

    let day23 () =
        let prog =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine
        let comp = run (prog, (Computer.Create()))
        comp.Registers.['a']
