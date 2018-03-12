namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Instruction =

    [<Literal>]
    let MaximumLocalVariables = 15
    
    type OpcodeForm =
      | Long
      | Short
      | Variable
      | Extended

    type OperandCount =
      | OP0
      | OP1
      | OP2
      | VAR

    let decodeVariable n =
      if n = 0 then Stack
      else if n <= MaximumLocalVariables then Local (LocalVariable n)
      else Global (GlobalVariable n)

    let encodeVariable variable =
      match variable with
      | Stack -> 0
      | Local (LocalVariable n) -> n
      | Global (GlobalVariable n) -> n

    (* We match Inform's convention of numbering the locals and globals from zero *)
    let displayVariable variable =
      match variable with
      | Stack -> "sp"
      | Local (LocalVariable n) -> sprintf "local%d" (n - 1)
      | Global (GlobalVariable n) -> sprintf "g%02x" (n - 16)

    type OperandType =
      | LargeOperand
      | SmallOperand
      | VariableOperand
      | Omitted

    (* The tables which follow are maps from the opcode identification number
       to the opcode type; the exact order matters. *)

    let one_operand_bytecodes = [|
      OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
      OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143  |]

    let zero_operand_bytecodes = [|
      OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
      OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  |]

    let two_operand_bytecodes =[|
      ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
      OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
      OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
      OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL |]

    let var_operand_bytecodes = [|
      VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
      VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
      VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
      VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 |]

    let ext_bytecodes = [|
      EXT_0;   EXT_1;   EXT_2;   EXT_3;   EXT_4;   EXT_5;   EXT_6;   EXT_7;
      EXT_8;   EXT_9;   EXT_10;  EXT_11;  EXT_12;  EXT_13;  EXT_14;  ILLEGAL;
      EXT_16;  EXT_17;  EXT_18;  EXT_19;  EXT_20;  EXT_21;  EXT_22;  EXT_23;
      EXT_24;  EXT_25;  EXT_26;  EXT_27;  EXT_28;  EXT_29;  ILLEGAL; ILLEGAL |]

    type T = {
      opcode : OpCode;
      address : instruction_address;
      length : int;
      operands : operand list;
      store : variable_location option;
      branch : (bool * branch_address) option;
      text : string option;
    }

    let address instruction =
      instruction.address

    let length instruction =
      instruction.length

    let opcode instruction =
      instruction.opcode

    let operands instruction =
      instruction.operands

    let store instruction =
      instruction.store

    let branch instruction =
      instruction.branch

    let text instruction =
      instruction.text
