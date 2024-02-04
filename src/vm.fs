module VM

open System.Collections.Generic

open Tokens

type State =
    { pc  : int
      acc : int
      ix  : int
      mar : int
      mdr : int
      flag: int
      memory: Dictionary<int, int> }
    static member Default =
        { pc     = 0
          acc    = 0
          ix     = 0
          mar    = 0
          mdr    = 0
          flag   = 0
          memory = Dictionary () }

let matchNum = function
    | Number num   -> num
    | Address addr -> addr
    | Register reg -> failwith "Expected a number, got a register: {reg}" 

let getMem state addr =
    let content = ref 0
    match state.memory.TryGetValue (addr, content) with
    | true  -> content.Value
    | false -> failwith $"Invalid memory access: address '{addr}' has not been set"

let regAdd state num = function
    | ACC -> { state with acc = state.acc + num }
    | IX  -> { state with ix  = state.ix  + num }
let regAddIndirect state addr = function
    | ACC -> { state with acc = state.acc + getMem state addr }
    | IX  -> { state with ix  = state.ix  + getMem state addr }

let run (program: Program): unit =
    let program = { program with start = 77 }
    printfn $"Starting program:\n{program}\n"
    let rec loop state =
        let pc   = state.pc
        let next = { state with pc = pc + 1 }
        printfn $"[{pc}]: {program.instrs[pc]}"
        match program.instrs[state.pc] with
        | LDM num  -> loop { next with acc = num }
        | LDD addr -> loop { next with acc = getMem state addr }
        | LDI addr -> loop { next with acc = getMem state (getMem state addr) }
        | LDX addr -> loop { next with ix  = getMem state addr }
        | LDR num  -> loop { next with ix  = num }
        | MOV reg  ->
            match reg with
            | ACC -> loop next
            | IX  -> loop { next with ix = state.acc }
        | STO addr ->
            state.memory[addr] <- state.acc
            loop next
        | ADD oper -> loop (regAdd next  (matchNum oper) ACC)
        | SUB oper -> loop (regAdd next -(matchNum oper) ACC)
        | INC reg  -> loop (regAdd next  1 reg)
        | DEC reg  -> loop (regAdd next -1 reg)
        | JMP addr -> loop { state with pc = addr - program.start }
        | CMP oper -> loop { next with flag = 0 } // TODO
        | CMI addr -> loop { next with flag = 0 } // TODO
        | JPE addr -> loop { state with pc = if state.flag = 0 then addr - program.start
                                                               else state.pc + 1 }
        | JPN addr -> loop { state with pc = if state.flag <> 0 then addr - program.start
                                                                else state.pc + 1 }
        | IN -> loop next
        | OUT ->
            printfn $"{char state.acc}"
            loop next
        | END -> ()
        | instr -> failwith $"Unhandled instruction: {instr} \n {state}" 

    loop State.Default
