namespace ASAEmulator

open System
open System.IO
open System.Globalization

open Tokens

module Parser =
    let ( >|= ) = Result.map

    let parseNumber (str: string) =
        try assert (str.Chars 0 = '#')
            Ok (Int32.Parse (str.Substring 1, NumberStyles.Integer))
        with exn -> Error $"Expected a #number, got: '{str}'"

    let parseAddress str =
        try Ok (Int32.Parse (str, NumberStyles.Integer))
        with exn -> Error $"Expected an address, got: '{str}'"

    let parseNumOrAddr (str: string) =
        let prefix = str.Chars 0
        let tail   = str.Substring 1
        try match prefix with
            | 'B' -> Ok (Number <| Int32.Parse (tail, NumberStyles.BinaryNumber))
            | '&' -> Ok (Number <| Int32.Parse (tail, NumberStyles.HexNumber))
            | '#' -> Number  >|= (parseNumber  str)
            | _   -> Address >|= (parseAddress str)
        with exn -> Error $"Failed to parse number: '{str}' ({exn})"

    let parseRegister (str: string) =
        match str.ToLower () with
        | "acc" -> Ok ACC
        | "ix"  -> Ok IX
        | str   -> Error $"Expected a register (ACC or IX), got: '{str}'"

    let parseOperand str =
        try parseNumOrAddr str
        with exn ->
            try Register >|= (parseRegister str)
            with exn -> Error $"Expected an operand (Register or number), got: {str}"

    let parseInstr instr oper =
        match instr, oper with
        | "LDM", Some oper -> LDM >|= (parseNumber    oper)
        | "LDD", Some oper -> LDD >|= (parseAddress   oper)
        | "LDI", Some oper -> LDI >|= (parseAddress   oper)
        | "LDX", Some oper -> LDX >|= (parseAddress   oper)
        | "LDR", Some oper -> LDR >|= (parseNumber    oper)
        | "MOV", Some oper -> MOV >|= (parseRegister  oper)
        | "STO", Some oper -> STO >|= (parseAddress   oper)
        | "ADD", Some oper -> ADD >|= (parseNumOrAddr oper)
        | "SUB", Some oper -> SUB >|= (parseNumOrAddr oper)
        | "INC", Some oper -> INC >|= (parseRegister  oper)
        | "DEC", Some oper -> DEC >|= (parseRegister  oper)
        | "JMP", Some oper -> JMP >|= (parseAddress   oper)
        | "CMP", Some oper -> CMP >|= (parseOperand   oper)
        | "CMI", Some oper -> CMI >|= (parseAddress   oper)
        | "JPE", Some oper -> JPE >|= (parseAddress   oper)
        | "JPN", Some oper -> JPN >|= (parseAddress   oper)
        | "IN" , None -> Ok IN
        | "OUT", None -> Ok OUT
        | "END", None -> Ok END
        | "AND", Some oper -> AND >|= (parseOperand oper)
        | "OR" , Some oper -> OR  >|= (parseOperand oper)
        | "XOR", Some oper -> XOR >|= (parseOperand oper)
        | "LSL", Some oper -> LSL >|= (parseNumber  oper)
        | "LSR", Some oper -> LSR >|= (parseNumber  oper)
        | instr, oper -> Error $"Unrecognized values: '{instr}' '{oper}'"

    let parseInstrs (instrs: (string * string option) list) (memory: (int * int) list) start =
        let instrs =
            instrs
            |> List.map (fun (instr, oper) ->
                parseInstr instr oper)
            |> List.mapi (fun i res ->
                match res with
                | Ok instr  -> instr
                | Error err -> raise (invalidArg "Instruction" $"[{i}] '{err}'"))
            |> Array.ofList
        let memory = memory |> Array.ofList
        { instrs = instrs
          memory = memory
          start  = start }

#if !FABLE_COMPILER
    let parseFile (file: StreamReader): Program =
        let rec parseMemory start memory lNum =
            if file.Peek () = int ';' then
                file.ReadLine()
                    .Trim(' ', '\t', ';')
                    .Split(' ')
                |> function
                | [| "start"; value |] -> parseMemory (int value) memory (lNum + 1)
                | [| addr; value |] ->
                    let addr  = int addr
                    let value = int value
                    parseMemory start ((addr, value)::memory) (lNum + 1)
                | xs -> failwith $"Memory on line {lNum} appears to be malformed.
                                   It should have the form: '; <addr> <value>'
                                   where <addr> and <value> are integers."
            else start, memory, lNum

        let rec parseProgram instrs lNum =
            match file.ReadLine () with
            | null -> instrs
            | "\n"
            | ""   -> parseProgram instrs (lNum + 1)
            | line ->
                let ci = match line.Split " " with
                         | [| instr; oper |] -> parseInstr instr (Some oper)
                         | [| instr |] -> parseInstr instr None
                         | xs -> failwith $"Unrecognized: {xs}"
                match ci with
                | Ok    ci  -> parseProgram ((lNum, ci)::instrs) (lNum + 1)
                | Error err -> failwith $"Error parsing line {lNum}: {err}"

        let start, memory, lNum = parseMemory 1 [] 1
        let instrs = parseProgram [] lNum
        { instrs = instrs
                   |> List.map (fun (i, instr) -> instr)
                   |> Array.ofList
                   |> Array.rev
          memory = memory |> Array.ofList
          start  = start }
#endif

    let parse (file: string array): Program =
        let rec parseMemory start memory lNum =
            if file.Peek () = int ';' then
                file.ReadLine()
                    .Trim(' ', '\t', ';')
                    .Split(' ')
                |> function
                | [| "start"; value |] -> parseMemory (int value) memory (lNum + 1)
                | [| addr; value |] ->
                    let addr  = int addr
                    let value = int value
                    parseMemory start ((addr, value)::memory) (lNum + 1)
                | xs -> failwith $"Memory on line {lNum} appears to be malformed.
                                   It should have the form: '; <addr> <value>'
                                   where <addr> and <value> are integers."
            else start, memory, lNum

        let rec parseProgram instrs lNum =
            match file.ReadLine () with
            | null -> instrs
            | "\n"
            | ""   -> parseProgram instrs (lNum + 1)
            | line ->
                let ci = match line.Split " " with
                         | [| instr; oper |] -> parseInstr instr (Some oper)
                         | [| instr |] -> parseInstr instr None
                         | xs -> failwith $"Unrecognized: {xs}"
                match ci with
                | Ok    ci  -> parseProgram ((lNum, ci)::instrs) (lNum + 1)
                | Error err -> failwith $"Error parsing line {lNum}: {err}"

        let start, memory, lNum = parseMemory 1 [] 1
        let instrs = parseProgram [] lNum
        { instrs = instrs
                   |> List.map (fun (i, instr) -> instr)
                   |> Array.ofList
                   |> Array.rev
          memory = memory |> Array.ofList
          start  = start }
