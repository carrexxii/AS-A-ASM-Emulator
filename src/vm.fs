module VM

open System.Collections.Generic

open Tokens

type State =
    { program: Program
      pc     : int
      acc    : int
      ix     : int
      mar    : int
      mdr    : int
      cir    : Instruction
      flag   : int
      memory : Dictionary<int, int>
      output : string -> unit }
    static member Default =
        { program = Program.Default
          pc      = 0
          acc     = 0
          ix      = 0
          mar     = 0
          mdr     = 0
          cir     = START
          flag    = 0
          memory  = Dictionary ()
          output  = fun str -> printfn $"{str}" }

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

let step state =
    let pc    = state.pc
    let state = { state with cir = state.program.instrs[state.pc] }
    let next  = { state with pc = pc + 1 }
    printfn $"[%2d{pc + state.program.start}]: {state.cir, -32}| (ACC = %3d{state.acc}, IX = %2d{state.ix}, flag=%2d{state.flag})"
    match state.cir with
    | LDM num  -> { next with acc = num }
    | LDD addr -> { next with acc = getMem state.memory addr }
    | LDI addr -> { next with acc = getMem state.memory (getMem state.memory addr) }
    | LDX addr -> { next with acc = getMem state.memory (addr + state.ix) }
    | LDR num  -> { next with ix  = num }
    | MOV reg  ->
        match reg with
        | ACC -> next
        | IX  -> { next with ix = state.acc }
    | STO addr ->
        state.memory[addr] <- state.acc
        next
    | ADD oper -> (regAdd next (matchNum oper) ACC false)
    | SUB oper -> (regAdd next (matchNum oper) ACC true)
    | INC reg  -> (regAdd next (Number 1) reg false)
    | DEC reg  -> (regAdd next (Number 1) reg true)
    | JMP addr -> { state with pc = addr - state.program.start }
    | CMP oper ->
        { next with
            flag = match oper with
                   | Number x when x = state.acc -> 0
                   | Number x when x < state.acc -> -1
                   | Number x when x > state.acc -> 1
                   | x -> failwith $"Unexpected value for compare: '{x}'\n{state}" }
    | CMI addr ->
        let oper = getMem state.memory addr
        { next with
            flag = match oper with
                   | x when x = state.acc -> 0
                   | x when x < state.acc -> -1
                   | x when x > state.acc -> 1 }
    | JPE addr -> { state with pc = if state.flag = 0
                                    then addr - state.program.start
                                    else state.pc + 1 }
    | JPN addr -> { state with pc = if state.flag <> 0
                                    then addr - state.program.start
                                    else state.pc + 1 }
    | IN  -> next
    | OUT ->
        state.output (char state.acc |> string)
        next
    | END   -> state
    | instr -> failwith $"Unhandled instruction: {instr} \n {state}" 

let load output (program: Program) = 
    let state = { State.Default with
                    program = program
                    output  = output }
    program.memory
    |> Array.iter (fun (addr: int, value: int) ->
        state.memory[addr] <- value)
    state

let rec run state =
    if state.cir <> END
    then step state |> run
    else ()
