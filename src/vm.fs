module VM

open System.Collections.Generic

open Tokens

type State =
    { pc  : int
      acc : int
      ix  : int
      mar : int
      mdr : int
      cir : Instruction
      flag: int
      memory: Dictionary<int, int> }
    static member Default =
        { pc     = 0
          acc    = 0
          ix     = 0
          mar    = 0
          mdr    = 0
          cir    = END
          flag   = 0
          memory = Dictionary () }

let matchNum = function
    | Number num   -> Number num
    | Address addr -> Address addr
    | Register reg -> failwith "Expected a number, got a register: {reg}" 

let getMem (memory: Dictionary<int, int>) addr =
    let content = ref 0
    match memory.TryGetValue (addr, content) with
    | true  -> content.Value
    | false -> failwith $"Invalid memory access: address '{addr}' has not been set"

let regAdd state num reg neg =
    let num = match num with
              | Number  num  -> num
              | Address addr -> getMem state.memory addr
              | Register reg -> failwith $"Unexpected register argument for regAdd: {reg}\n{state}"
    match reg with
    | ACC -> { state with acc = if not neg then state.acc + num else state.acc - num }
    | IX  -> { state with ix  = if not neg then state.ix  + num else state.ix  - num }

let regAddIndirect state addr = function
    | ACC -> { state with acc = state.acc + getMem state.memory addr }
    | IX  -> { state with ix  = state.ix  + getMem state.memory addr }

let run (program: Program): unit =
    let rec loop state =
        let pc   = state.pc
        let next = { state with pc = pc + 1 }
        printfn $"[%2d{pc + program.start}]: {program.instrs[pc], -32}| (ACC = %3d{state.acc}, IX = %2d{state.ix}, flag=%2d{state.flag})"
        match program.instrs[state.pc] with
        | LDM num  -> loop { next with acc = num }
        | LDD addr -> loop { next with acc = getMem state.memory addr }
        | LDI addr -> loop { next with acc = getMem state.memory (getMem state.memory addr) }
        | LDX addr -> loop { next with acc = getMem state.memory (addr + state.ix) }
        | LDR num  -> loop { next with ix  = num }
        | MOV reg  ->
            match reg with
            | ACC -> loop next
            | IX  -> loop { next with ix = state.acc }
        | STO addr ->
            printfn $"\tMemory before: {state.memory[addr]}"
            state.memory[addr] <- state.acc
            printfn $"\tMemory after: {state.memory[addr]}"
            loop next
        | ADD oper -> loop (regAdd next (matchNum oper) ACC false)
        | SUB oper -> loop (regAdd next (matchNum oper) ACC true)
        | INC reg  -> loop (regAdd next (Number 1) reg false)
        | DEC reg  -> loop (regAdd next (Number 1) reg true)
        | JMP addr -> loop { state with pc = addr - program.start }
        | CMP oper ->
            loop { next with
                     flag = match oper with
                            | Number x when x = state.acc -> 0
                            | Number x when x < state.acc -> -1
                            | Number x when x > state.acc -> 1
                            | x -> failwith $"Unexpected value for compare: '{x}'\n{state}" }
        | CMI addr ->
            let oper = getMem state.memory addr
            loop { next with
                     flag = match oper with
                            | x when x = state.acc -> 0
                            | x when x < state.acc -> -1
                            | x when x > state.acc -> 1 }
        | JPE addr -> loop { state with pc = if state.flag = 0
                                             then addr - program.start
                                             else state.pc + 1 }
        | JPN addr -> loop { state with pc = if state.flag <> 0
                                             then addr - program.start
                                             else state.pc + 1 }
        | IN -> loop next
        | OUT ->
            printfn $"{state.acc} -> '{char state.acc}'"
            loop next
        | END -> ()
        | instr -> failwith $"Unhandled instruction: {instr} \n {state}" 

    let state = State.Default
    program.memory |> Array.iter (fun (addr: int, value: int) ->
                                     state.memory[addr] <- value)
    printfn $"Starting program:\n{program}\n"
    loop state
