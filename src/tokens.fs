namespace ASAEmulator

module Tokens =
    type Label   = string
    type Number  = int
    type Address = int

    type Register =
        | ACC
        | IX

    type Operand =
        | Number   of Number
        | Address  of Address
        | Register of Register

    type Instruction =
        | LDM of Number
        | LDD of Address
        | LDI of Address
        | LDX of Address
        | LDR of Number
        | MOV of Register
        | STO of Address
        | ADD of Operand
        | SUB of Operand
        | INC of Register
        | DEC of Register
        | JMP of Address
        | CMP of Operand
        | CMI of Address
        | JPE of Address
        | JPN of Address
        | IN
        | OUT
        | START
        | END

        | AND of Operand
        | OR  of Operand
        | XOR of Operand
        | LSL of Number
        | LSR of Number

    type Program =
        { instrs: Instruction array
          memory: (int * int) array
          start : int }
        static member Default =
            { instrs = [||]
              memory = [||]
              start  = 0 }
