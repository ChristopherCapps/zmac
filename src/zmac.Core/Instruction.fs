namespace Zmac.Core

open Type
open Utility
open Machine
open Machine.Memory

module Instruction =

  // ****** Types that comprise a complete Z-machine instruction ******

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

  type T = {
    address: InstructionAddress
    opcode: OpCode
    form: OpcodeForm
    operandTypes: OperandType list
    operands: Operand list
    store: Variable option
    branch: (bool*BranchAddress) option
    text: string option
    length: int
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

  let isCall ver opcode =
    match opcode with
    | OP1_143 -> ver >= Version5  (* call_1n in v5, logical not in v1-4 *)
    | VAR_224 (* call / call_vs *)
    | OP1_136 (* call_1s *)
    | OP2_26  (* call_2n *)
    | OP2_25  (* call_2s *)
    | VAR_249 (* call_vn *)
    | VAR_250 (* call_vn2 *)
    | VAR_236 (* call_vs2 *) -> true
    | _ -> false

  // let call_address instr story  =
  //   if is_call (Story.version story) instr.opcode then
  //     match instr.operands with
  //     | (Large packed_address) :: _ ->
  //       let packed_address = Packed_routine packed_address in
  //       let unpacked_address = Story.decode_routine_packed_address story packed_address in
  //       Some unpacked_address
  //     | _ -> None
  //   else
  //     None

  let hasStore opcode ver =
    match opcode with
    | OP1_143 -> ver <= Version4 (* call_1n in v5, logical not in v1-4 *)
    | OP0_181 -> ver >= Version4 (* save branches in v3, stores in v4 *)
    | OP0_182 -> ver >= Version4 (* restore branches in v3, stores in v4 *)
    | OP0_185 -> ver >= Version4 (* pop in v4, catch in v5 *)
    | VAR_233 -> ver = Version6
    | VAR_228 -> ver >= Version5
    | OP2_8   | OP2_9   | OP2_15  | OP2_16  | OP2_17  | OP2_18  | OP2_19
    | OP2_20  | OP2_21  | OP2_22  | OP2_23  | OP2_24  | OP2_25
    | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_136 | OP1_142
    | VAR_224 | VAR_231 | VAR_236 | VAR_246 | VAR_247 | VAR_248
    | EXT_0   | EXT_1   | EXT_2   | EXT_3   | EXT_4   | EXT_9
    | EXT_10  | EXT_19  | EXT_29 -> true
    | _ -> false

  let continuesToNext opcode =
    match opcode with
    | OP2_28 (* throw *)
    | OP1_139 (* ret *)
    | OP1_140 (* jump *)
    | OP0_176 (* rtrue *)
    | OP0_177 (* rfalse *)
    | OP0_179 (* print_ret *)
    | OP0_183 (* restart *)
    | OP0_184 (* ret_popped *)
    | OP0_186 (* quit *) -> false
    | _ -> true

  let hasText opcode =
    match opcode with
    | OP0_178 | OP0_179 -> true
    | _ -> false

  let hasBranch opcode ver =
    match opcode with
    | OP0_181 -> ver <= Version3  (* save branches in v3, stores in v4 *)
    | OP0_182 -> ver <= Version3  (* restore branches in v3, stores in v4 *)
    | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7   | OP2_10
    | OP1_128 | OP1_129 | OP1_130 | OP0_189 | OP0_191
    | VAR_247 | VAR_255
    | EXT_6   | EXT_14 | EXT_24  | EXT_27 -> true
    | _ -> false
    
  let hasIndirection instruction ver = 
      match instruction.opcode with
      | VAR_233 when ver = Version6 -> false  (* pull *)
      | OP2_4   (* dec_chk *)
      | OP2_5   (* inc_chk *)
      | OP2_13  (* store *)
      | OP1_133 (* inc *)
      | OP1_134 (* dec *)
      | OP1_142 (* load *)
      | VAR_233 (* pull *)
        -> true
      | _ -> false

  let opcodeName opcode ver =
    match opcode with
    | ILLEGAL -> "ILLEGAL"
    | OP2_1   -> "je"
    | OP2_2   -> "jl"
    | OP2_3   -> "jg"
    | OP2_4   -> "dec_chk"
    | OP2_5   -> "inc_chk"
    | OP2_6   -> "jin"
    | OP2_7   -> "test"
    | OP2_8   -> "or"
    | OP2_9   -> "and"
    | OP2_10  -> "test_attr"
    | OP2_11  -> "set_attr"
    | OP2_12  -> "clear_attr"
    | OP2_13  -> "store"
    | OP2_14  -> "insert_obj"
    | OP2_15  -> "loadw"
    | OP2_16  -> "loadb"
    | OP2_17  -> "get_prop"
    | OP2_18  -> "get_prop_addr"
    | OP2_19  -> "get_next_prop"
    | OP2_20  -> "add"
    | OP2_21  -> "sub"
    | OP2_22  -> "mul"
    | OP2_23  -> "div"
    | OP2_24  -> "mod"
    | OP2_25  -> "call_2s"
    | OP2_26  -> "call_2n"
    | OP2_27  -> "set_colour"
    | OP2_28  -> "throw"
    | OP1_128 -> "jz"
    | OP1_129 -> "get_sibling"
    | OP1_130 -> "get_child"
    | OP1_131 -> "get_parent"
    | OP1_132 -> "get_prop_len"
    | OP1_133 -> "inc"
    | OP1_134 -> "dec"
    | OP1_135 -> "print_addr"
    | OP1_136 -> "call_1s"
    | OP1_137 -> "remove_obj"
    | OP1_138 -> "print_obj"
    | OP1_139 -> "ret"
    | OP1_140 -> "jump"
    | OP1_141 -> "print_paddr"
    | OP1_142 -> "load"
    | OP1_143 -> if ver <= Version4 then "not" else "call_1n"
    | OP0_176 -> "rtrue"
    | OP0_177 -> "rfalse"
    | OP0_178 -> "print"
    | OP0_179 -> "print_ret"
    | OP0_180 -> "nop"
    | OP0_181 -> "save"
    | OP0_182 -> "restore"
    | OP0_183 -> "restart"
    | OP0_184 -> "ret_popped"
    | OP0_185 -> if ver <= Version4 then "pop" else "catch"
    | OP0_186 -> "quit"
    | OP0_187 -> "new_line"
    | OP0_188 -> "show_status"
    | OP0_189 -> "verify"
    | OP0_190 -> "EXTENDED"
    | OP0_191 -> "piracy"
    | VAR_224 -> if ver <= Version3 then "call" else "call_vs"
    | VAR_225 -> "storew"
    | VAR_226 -> "storeb"
    | VAR_227 -> "put_prop"
    | VAR_228 -> if ver <= Version4 then "sread" else "aread"
    | VAR_229 -> "print_char"
    | VAR_230 -> "print_num"
    | VAR_231 -> "random"
    | VAR_232 -> "push"
    | VAR_233 -> "pull"
    | VAR_234 -> "split_window"
    | VAR_235 -> "set_window"
    | VAR_236 -> "call_vs2"
    | VAR_237 -> "erase_window"
    | VAR_238 -> "erase_line"
    | VAR_239 -> "set_cursor"
    | VAR_240 -> "get_cursor"
    | VAR_241 -> "set_text_style"
    | VAR_242 -> "buffer_mode"
    | VAR_243 -> "output_stream"
    | VAR_244 -> "input_stream"
    | VAR_245 -> "sound_effect"
    | VAR_246 -> "read_char"
    | VAR_247 -> "scan_table"
    | VAR_248 -> "not"
    | VAR_249 -> "call_vn"
    | VAR_250 -> "call_vn2"
    | VAR_251 -> "tokenise"
    | VAR_252 -> "encode_text"
    | VAR_253 -> "copy_table"
    | VAR_254 -> "print_table"
    | VAR_255 -> "check_arg_count"
    | EXT_0   -> "save"
    | EXT_1   -> "restore"
    | EXT_2   -> "log_shift"
    | EXT_3   -> "art_shift"
    | EXT_4   -> "set_font"
    | EXT_5   -> "draw_picture"
    | EXT_6   -> "picture_data"
    | EXT_7   -> "erase_picture"
    | EXT_8   -> "set_margins"
    | EXT_9   -> "save_undo"
    | EXT_10  -> "restore_undo"
    | EXT_11  -> "print_unicode"
    | EXT_12  -> "check_unicode"
    | EXT_13  -> "set_true_colour"
    | EXT_14  -> "sound_data"
    | EXT_16  -> "move_window"
    | EXT_17  -> "window_size"
    | EXT_18  -> "window_style"
    | EXT_19  -> "get_wind_prop"
    | EXT_20  -> "scroll_window"
    | EXT_21  -> "pop_stack"
    | EXT_22  -> "read_mouse"
    | EXT_23  -> "mouse_window"
    | EXT_24  -> "push_stack"
    | EXT_25  -> "put_wind_prop"
    | EXT_26  -> "print_form"
    | EXT_27  -> "make_menu"
    | EXT_28  -> "picture_table"
    | EXT_29  -> "buffer_screen"    

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

  // ****** Instruction decoding ******

  let decode machine (InstructionAddress address) = 
    let version = version machine

    ///////// OPERAND FORM
    // Decode form of the opcode, stored in the high two bits
    let decodeOpcodeForm address =
      let instruction = readByte machine address
      match (readBits BitNumber7 BitCount2 instruction) with
        | 0b11 -> VariableForm
        | 0b10 -> ShortForm
        | 0xBE when (version >= Version5) -> ExtendedForm
        | _ -> LongForm

    ///////// OPERAND COUNT
    // Number of operands depends on form and instruction bits
    let decodeOperandCount address opcodeForm =
      let instruction = readByte machine address
      match opcodeForm with
      // We have 1 or 2 operands in short form, with the type also represented
      | ShortForm -> 
          match (readBits BitNumber5 BitCount2 instruction) with
          | 0b11 -> ZeroOperands
          | _   -> OneOperand

      // In long form we always have 2 operands
      | LongForm -> TwoOperands

      // With variable form, we have either 2 or variable operands based on bit 5
      | VariableForm -> 
          match (readBit BitNumber5 instruction) with
          | false -> TwoOperands
          | true -> VariableOperands

      // In extended form we always have variable operands
      | ExtendedForm -> VariableOperands

    ///////// OPCODE MAPPING
    // Map the opcode number and operand count to a specific opcode type
    let decodeOpcode address opcodeForm operandCount =
      let instruction = readByte machine address
      let operandFamily = 
        match operandCount with
        | ZeroOperands -> ZeroOperandOpCodes
        | OneOperand -> OneOperandOpCodes
        | TwoOperands -> TwoOperandOpCodes
        | VariableOperands -> VariableOperandOpCodes
      let opcodeNumber = 
        match opcodeForm with
        | ShortForm -> readBits BitNumber3 BitCount4 instruction
        | LongForm | VariableForm -> readBits BitNumber4 BitCount5 instruction
        | ExtendedForm -> readByte machine (incrementByteAddress address)
      if opcodeNumber >= 0 && opcodeNumber < operandFamily.Length then 
        let operation' = operandFamily.[opcodeNumber]
        if operation' = ILLEGAL then 
          failwithf "Illegal opcode specified: opcode count %A, opcode %A" operandCount opcodeNumber
        else operation'
      else failwithf "Illegal opcode specified: opcode count %A, opcode %A" operandCount opcodeNumber
    
    let opcodeLength = function
      | ExtendedForm -> 2
      | _ -> 1

    let decodeOperandType operandType =
      match operandType with
      | 0b00 -> LargeOperand
      | 0b01 -> SmallOperand
      | 0b10 -> VariableOperand
      | 0b11 -> Omitted
      | _ -> failwithf "Unexpected operand type: %d" operandType

    let unpackOperandTypes b = [ for i = 0 to 3 do yield (readBits (BitNumber (7-i*2)) BitCount2 b) ]

    let decodeOperandTypes address opcodeForm opcode = 
      match opcodeForm with
      | ShortForm ->
          readByte machine address
          |> readBits BitNumber5 BitCount2
          |> decodeOperandType
          |> function
              | Omitted -> []
              | operandType -> [ operandType ]

      | LongForm -> 
          let instruction = readByte machine address
          [ BitNumber6; BitNumber5 ]
          |> List.map (fun bit -> 
              readBits bit BitCount1 instruction
              |> function
                  | 0 -> SmallOperand
                  | _ -> VariableOperand) // Can only be 1

      | ExtendedForm | VariableForm -> 
          let packedOperandTypesAddress = incrementByteAddress address
          let packedOperandTypes = readByte machine packedOperandTypesAddress 
      
          let unpackedOperandTypes = 
            let unpackedOperandTypes' = unpackOperandTypes packedOperandTypes
            if opcode = EXT_12 || opcode = EXT_26 then // double VAR, up to 8 operands
              let extPackedOperandTypesAddress = incrementByteAddress packedOperandTypesAddress
              let extPackedOperandTypes = readByte machine extPackedOperandTypesAddress 
              unpackedOperandTypes' @ (unpackOperandTypes extPackedOperandTypes)
            else
              unpackedOperandTypes'                

          unpackedOperandTypes
          |> List.map decodeOperandType
          |> List.filter (((=) Omitted) >> not)

    let operandTypesLength opcodeForm opcode =
      match opcodeForm with 
      | ShortForm | LongForm -> 0
      | ExtendedForm when opcode = EXT_12 || opcode = EXT_26 -> 2
      | VariableForm | ExtendedForm -> 1

    let decodeOperands address operandTypes =
      let rec loop operandTypes' address' acc =
        //printfn "loop with %A and %A and %A" operandTypes' address' acc
        match operandTypes' with
        | [] -> acc
        | operandType::tail -> 
            match operandType with
            | LargeOperand -> 
                let operand = LargeConstant (readWord machine (byteAddressToWordAddress address'))
                loop tail (incrementByteAddressBy WordLength address') (acc @ [operand])
            | SmallOperand ->
                let operand = SmallConstant (readByte machine address')
                loop tail (incrementByteAddress address') (acc @ [operand])
            | VariableOperand ->
                let operand = Variable (readByte machine address' |> decodeVariable)
                loop tail (incrementByteAddress address') (acc @ [operand])
            | Omitted -> 
                failwithf "Operand type 'OMITTED' cannot be decoded"

      loop operandTypes address []

    let operandsLength operandTypes =
      operandTypes
      |> List.fold (fun length operandType -> length + (if operandType = LargeOperand then WordLength else 1)) 0

    let decodeStore storeAddress opcode =
      if hasStore opcode version then
        readByte machine storeAddress
        |> decodeVariable
        |> Some
      else
        None

    let storeLength opcode =
      if hasStore opcode version then 1 else 0

    let branchLength branchAddress opcode =
      if hasBranch opcode version then
        readByte machine branchAddress
        |> readBit BitNumber6
        |> function
            | true -> 1
            | false -> 2
      else 0

    let decodeBranch branchAddress opcode =
      if hasBranch opcode version then
        let high = readByte machine branchAddress
        let branchConditionResult = readBit BitNumber7 high
        let branchLength = branchLength branchAddress opcode

        let offset =
          let high' = readBits BitNumber5 BitCount6 high
          if (readBit BitNumber6 high) then
            high'
          else
            let low = readByte machine (incrementByteAddress branchAddress)
            let unsigned = 256 * high' + low
            if unsigned < 8192 then unsigned else unsigned - 16384

        match offset with
        | 0 -> Some (branchConditionResult, ReturnFalse)
        | 1 -> Some (branchConditionResult, ReturnTrue)
        | _ ->             
              let (ByteAddress addressAfterBranchData) = incrementByteAddressBy branchLength branchAddress
              let instructionAddress = InstructionAddress (addressAfterBranchData + offset - 2)
              Some (branchConditionResult, BranchAddress instructionAddress)
      else
        None

    let decodeText textAddress opcode =
      if hasText opcode then
        Some (Text.readZString machine textAddress)
      else 
        None

    let textLength textAddress opcode =
      if hasText opcode then Text.encodedLength machine textAddress else 0

    let instructionBaseAddress = ByteAddress address
    let opcodeForm = decodeOpcodeForm instructionBaseAddress
    let operandCount = decodeOperandCount instructionBaseAddress opcodeForm
    let opcode = decodeOpcode instructionBaseAddress opcodeForm operandCount
    let opcodeLen = opcodeLength opcodeForm
    let operandTypes = decodeOperandTypes instructionBaseAddress opcodeForm opcode
    let operandTypesLen = operandTypesLength opcodeForm opcode
    let operandsAddress = incrementByteAddressBy (opcodeLen + operandTypesLen) instructionBaseAddress
    let operands = decodeOperands operandsAddress operandTypes
    let operandsLen = operandsLength operandTypes
    let storeAddress = incrementByteAddressBy operandsLen operandsAddress
    let store = decodeStore storeAddress opcode
    let storeLen = storeLength opcode
    let branchAddress = incrementByteAddressBy storeLen storeAddress
    let branch = decodeBranch branchAddress opcode
    let branchLen = branchLength branchAddress opcode
    let (ByteAddress branchAddress') = branchAddress
    let textAddress = ZStringAddress (branchAddress' + branchLen)
    let text = decodeText textAddress opcode
    let textLen = textLength textAddress opcode
    let length = opcodeLen + operandTypesLen + operandsLen + storeLen + branchLen + textLen

    { 
      address = InstructionAddress address
      opcode = opcode
      form = opcodeForm
      operandTypes = operandTypes
      operands = operands
      store = store
      branch = branch
      text = text
      length = length
    }

  let displayOperand operand =
    match operand with
    | LargeConstant large -> Printf.sprintf "%04X " large
    | SmallConstant small -> Printf.sprintf "%02X " small
    | Variable variable -> (displayVariable variable) + " " 
    
  // let displayJump instruction =
  //   (* For jumps, display the absolute target rather than the relative target. *)
  //   match instruction.operands with
  //   | [LargeConstant offset] -> 
  //     let offset = signed_word offset in
  //     let (InstructionAddress target) = jump_address instr offset in
  //     Printf.sprintf "%04x " target
  //   | _ -> accumulate_strings display_operand instr.operands
    
  // let display_call instr story =
  //   match call_address instr story with
  //     | Some (Routine addr) ->
  //       let routine = (Printf.sprintf "%04x " addr) in
  //       let args = accumulate_strings display_operand (List.tl instr.operands) in
  //       routine ^ args
  //     | _ -> accumulate_strings display_operand instr.operands

  let display machine instruction =   
    let version = version machine

    let displayOperands () =
      // if instruction.opcode = OP1_140 then displayJump instruction
      // else if isCall version instruction.opcode then displayCall instruction machine
      // else if hasIndirection instruction version then displayIndirect_operands instruction.operands
      // else 
      let sb = 
        instruction.operands
        |> List.fold (fun (sb:System.Text.StringBuilder) operand -> 
                        let operandStr = displayOperand operand
                        sb.Append(operandStr)) 
                        (new System.Text.StringBuilder())
      sb.ToString()

    let displayStore () =
      match instruction.store with
      | None -> ""
      | Some variable -> "->" + (displayVariable variable)

    let displayBranch () =
      match instruction.branch with
      | None -> ""
      | Some (true, ReturnFalse) -> "?false"
      | Some (false, ReturnFalse) -> "?~false"
      | Some (true, ReturnTrue) -> "?true"
      | Some (false, ReturnTrue) -> "?~true"
      | Some (true, BranchAddress (InstructionAddress address)) -> Printf.sprintf "?%04x" address
      | Some (false, BranchAddress (InstructionAddress address)) -> Printf.sprintf "?~%04x" address in

    let displayText () =
      match instruction.text with
      | None -> ""
      | Some str -> str in  

    let (InstructionAddress address) = instruction.address
    let name = opcodeName instruction.opcode version
    let operands = displayOperands() 
    let store = displayStore() 
    let branch = displayBranch() 
    let text = displayText() 
    sprintf "%04X: %s %s%s %s %s\n" address name operands store branch text
