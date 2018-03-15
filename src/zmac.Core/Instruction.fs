namespace Zmac.Core

open Type
open Utility
open Machine
open Machine.Memory

module Instruction =

    // ****** Types that comprise a complete Z-machine instructions ******

    type OpcodeForm =
      | LongForm
      | ShortForm
      | VariableForm
      | ExtendedForm

    type OperandType =
      | LargeOperand
      | SmallOperand
      | VariableOperand
      | Omitted

    type OperandCount =
      | ZeroOperands
      | OneOperand
      | TwoOperands
      | VariableOperands

    type Operand =
      | Constant of int
      | Variable of Variable

    type T = {
      opcode: OpCode
      form: OpcodeForm
      operands: OperandType array
      isBranch: bool
      isStore: bool
      isText: bool
    }

    (* The tables which follow are maps from the opcode identification number
       to the opcode type; the exact order matters. *)

    let OneOperandOpCodes = [|
      OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
      OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143 
    |]

    let ZeroOperandOpCodes = [|
      OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
      OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  
    |]

    let TwoOperandOpCodes =[|
      ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
      OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
      OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
      OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL 
    |]

    let VariableOperandOpCodes = [|
      VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
      VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
      VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
      VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 
    |]

    let ExtendedOpCodes = [|
      EXT_0;   EXT_1;   EXT_2;   EXT_3;   EXT_4;   EXT_5;   EXT_6;   EXT_7;
      EXT_8;   EXT_9;   EXT_10;  EXT_11;  EXT_12;  EXT_13;  EXT_14;  ILLEGAL;
      EXT_16;  EXT_17;  EXT_18;  EXT_19;  EXT_20;  EXT_21;  EXT_22;  EXT_23;
      EXT_24;  EXT_25;  EXT_26;  EXT_27;  EXT_28;  EXT_29;  ILLEGAL; ILLEGAL 
    |]

    // ****** Instruction decoding ******

    let decodeOperandType operandType =
      match operandType with
      | 0b00 -> LargeOperand
      | 0b01 -> SmallOperand
      | 0b10 -> VariableOperand
      | 0b11 -> Omitted
      | _ -> failwithf "Unexpected operand type: %d" operandType

    let instruction machine (InstructionAddress address) = 
      let baseAddress = ByteAddress address
      let instruction = readByte machine baseAddress

      let opcodeForm =
        match (readBits BitNumber7 BitCount2 instruction) with
          | 0b11 -> VariableForm
          | 0b10 -> ShortForm
          | 0xBE when (version machine) >= Version5 -> ExtendedForm
          | _ -> LongForm

      let operandCount =
        match opcodeForm with
        | ShortForm -> 
            match (readBits BitNumber5 BitCount2 instruction) with
            | 0b11 -> ZeroOperands
            | _ -> OneOperand
        | LongForm -> TwoOperands
        | VariableForm -> 
            match (readBit BitNumber5 instruction) with
            | false -> TwoOperands
            | true -> VariableOperands
        | ExtendedForm -> VariableOperands

      let opcodeNumber =
        match opcodeForm with
        | ShortForm -> readBits BitNumber3 BitCount4 instruction
        | LongForm | VariableForm -> readBits BitNumber4 BitCount5 instruction
        | ExtendedForm -> readByte machine (incrementByteAddress baseAddress)

      let opcode =
        let operandFamily = 
          match operandCount with
          | ZeroOperands -> ZeroOperandOpCodes
          | OneOperand -> OneOperandOpCodes
          | TwoOperands -> TwoOperandOpCodes
          | VariableOperands -> VariableOperandOpCodes
        if opcodeNumber >= 0 && opcodeNumber < operandFamily.Length then 
          let operation' = operandFamily.[opcodeNumber]
          if operation' = ILLEGAL then 
            failwithf "Illegal opcode specified: opcode count %A, opcode %A" operandCount opcodeNumber
          else operation'
        else failwithf "Illegal opcode specified: opcode count %A, opcode %A" operandCount opcodeNumber
      
      let operands =
        let operandsOffset, operandTypes = 
          match opcodeForm with
          | ShortForm ->         
            let operandType = decodeOperandType (readBits BitNumber5 BitCount2 opcodeNumber) // Bits 4&5 give type
            match operandType with
            | Omitted -> None, Array.empty
            | _ -> Some (address+1), [| operandType |]
          | LongForm -> 
            Some (address+1),
              [| readBits BitNumber6 BitCount1 opcodeNumber; readBits BitNumber5 BitCount1 opcodeNumber |]
              |> Array.map (function
                  | 0 -> SmallOperand
                  | _ -> VariableOperand // Can only be 1
                 )
          | ExtendedForm | VariableForm -> 
            let offset, encodedOperandTypes, operandTypeOffsets = 
              if opcode = EXT_12 || opcode = EXT_26 then // double VAR, up to 8 operands
                Some (address+4), readWord machine (WordAddress (address+2)), [| 15; 13; 11; 9; 7; 5; 3; 1 |]
              else // single VAR, up to 4 operands
                let offset = if opcodeForm = ExtendedForm then 3 else 2
                Some (address+offset), readByte machine (ByteAddress (address+1)), [| 7; 5; 3; 1 |]             
            offset,
              operandTypeOffsets          
              |> Array.map (fun i -> readBits (BitNumber i) BitCount2 encodedOperandTypes |> decodeOperandType)
              |> Array.filter (((=) Omitted) >> not)

        match operandsOffset with
        | Some offset -> 
            let rec loop operandTypes acc offset =
              match operandTypes with
              | [||] -> acc
              | [|operandType|]::ts -> 
                  let operand, size = decodeOperand operandType offset
                  loop ts acc@[|operand|] (offset+size)
            loop operandTypes Array.empty offset
        | None -> Array.empty

      { 
        opcode = opcode
        form = opcodeForm
        operands = operands
        isBranch = false
        isStore = false
        isText = false
      }

    // ****** Variable operand manipulations ******

    let decodeVariable n =
      if n = 0 then Stack
      else if n >= Zmac.Core.Locals.FirstLocalVariable && n <= Locals.LastLocalVariable then Local (LocalVariable n)
      else Global (GlobalVariable n)

    let encodeVariable variable =
      match variable with
      | Stack -> 0
      | Local (LocalVariable n) -> n
      | Global (GlobalVariable n) -> n

    // We match Inform's convention of numbering the locals and globals from zero 
    let displayVariable variable =
      match variable with
      | Stack -> "sp"
      | Local (LocalVariable n) -> sprintf "local%d" (n - Locals.FirstLocalVariable)
      | Global (GlobalVariable n) -> sprintf "g%02x" (n - Globals.FirstGlobalVariable)

