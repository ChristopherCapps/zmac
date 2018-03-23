namespace Zmac.Core

module Type =

    (* PRIMITIVES & ADDRESS TYPES *)

    // A typed reference to a specific bit in a byte or word
    type BitNumber = BitNumber of int
    // A typed reference to a number of consecutive bits
    type BitCount = BitCount of int

    // A Word is technically a 2-byte value, but represented internally as an int for convenience
    type ZWord = int
    // A Byte is technically a 1-byte value, but represented internally as an int for convenience
    type ZByte = int

    // An absolute Z-Machine address which can span the entire model
    type ByteAddress = ByteAddress of int
    type WordAddress = WordAddress of int

    (* HEADER TYPES *)

    type VersionAddress = VersionAddress of int
    type StaticMemoryAddress = StaticMemoryAddress of int
    type DictionaryAddress = DictionaryAddress of int
    type AbbreviationsTableAddress = AbbreviationsTableAddress of int
    type ObjectTableAddress = ObjectTableAddress of int
    type GlobalVariablesTableAddress = GlobalVariablesTableAddress of int
    type ReleaseNumber = ReleaseNumber of int
    type SerialNumber = SerialNumber of string
    
    (* VARIABLE TYPES *)

    type GlobalVariable = GlobalVariable of int
    type LocalVariable = LocalVariable of int

    type Variable = 
        | Local of LocalVariable
        | Global of GlobalVariable
        | Stack
    
    (* DICTIONARY TYPES *)

    type DictionaryEntry = DictionaryEntry of int
    type DictionaryEntryAddress = DictionaryEntryAddress of int

    (* ABBREVIATIONS TYPES *)

    type AbbreviationAddress = AbbreviationAddress of int
    type Abbreviation = Abbreviation of int

    (* OBJECT TABLE TYPES *)

    type ObjectPropertyDefaultsAddress = ObjectPropertyDefaultsAddress of int
    type ObjectPropertyAddress = ObjectPropertyAddress of int
    type ObjectProperty = ObjectProperty of int
    type ObjectAttribute = ObjectAttribute of int
    type ObjectTreeAddress = ObjectTreeAddress of int
    type Object = Object of int
    type ObjectPropertiesDataAddress = ObjectPropertiesDataAddress of int
    type ObjectPropertyDataSize = ObjectPropertyDataSize of int
    type ObjectPropertyDataAddress = ObjectPropertyDataAddress of int
    type ObjectPropertyData = ObjectPropertyData of ObjectProperty * ObjectPropertyDataSize * ObjectPropertyDataAddress
    type ObjectAddress = ObjectAddress of int
    type ObjectNumberAddress = ObjectNumberAddress of int    
    type ObjectAttributesAddress = ObjectAttributesAddress of int
    type ObjectAttributeAddress = ObjectAttributeAddress of int * BitNumber
    type ObjectPropertiesAddress = ObjectPropertiesAddress of int

    (* INSTRUCTION/OPCODE TYPES *)

    type InstructionAddress = InstructionAddress of int

    (* ZTEXT TYPES *)    

    type ZStringAddress = ZStringAddress of int

    type Operand =
      | LargeConstant of int
      | SmallConstant of int
      | Variable of Variable

    type BranchAddress =
      | ReturnTrue
      | ReturnFalse
      | BranchAddress of InstructionAddress

    let BitNumber0 = BitNumber 0
    let BitNumber1 = BitNumber 1
    let BitNumber2 = BitNumber 2
    let BitNumber3 = BitNumber 3
    let BitNumber4 = BitNumber 4
    let BitNumber5 = BitNumber 5
    let BitNumber6 = BitNumber 6
    let BitNumber7 = BitNumber 7
    let BitNumber8 = BitNumber 8
    let BitNumber9 = BitNumber 9
    let BitNumber10 = BitNumber 10
    let BitNumber11 = BitNumber 11
    let BitNumber12 = BitNumber 12
    let BitNumber13 = BitNumber 13
    let BitNumber14 = BitNumber 14
    let BitNumber15 = BitNumber 15
    
    let BitCount1 = BitCount 1
    let BitCount2 = BitCount 2
    let BitCount3 = BitCount 3
    let BitCount4 = BitCount 4
    let BitCount5 = BitCount 5
    let BitCount6 = BitCount 6
    let BitCount7 = BitCount 7
    let BitCount8 = BitCount 8

    type Version = Version1 | Version2 | Version3 | Version4 | Version5 | Version6

    //[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "IdentifiersMustNotContainUnderscores")>]
    type OpCode =      
      | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7
      | OP2_8   | OP2_9   | OP2_10  | OP2_11  | OP2_12  | OP2_13  | OP2_14  | OP2_15
      | OP2_16  | OP2_17  | OP2_18  | OP2_19  | OP2_20  | OP2_21  | OP2_22  | OP2_23
      | OP2_24  | OP2_25  | OP2_26  | OP2_27  | OP2_28
      | OP1_128 | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_133 | OP1_134 | OP1_135
      | OP1_136 | OP1_137 | OP1_138 | OP1_139 | OP1_140 | OP1_141 | OP1_142 | OP1_143
      | OP0_176 | OP0_177 | OP0_178 | OP0_179 | OP0_180 | OP0_181 | OP0_182 | OP0_183
      | OP0_184 | OP0_185 | OP0_186 | OP0_187 | OP0_188 | OP0_189 | OP0_190 | OP0_191
      | VAR_224 | VAR_225 | VAR_226 | VAR_227 | VAR_228 | VAR_229 | VAR_230 | VAR_231
      | VAR_232 | VAR_233 | VAR_234 | VAR_235 | VAR_236 | VAR_237 | VAR_238 | VAR_239
      | VAR_240 | VAR_241 | VAR_242 | VAR_243 | VAR_244 | VAR_245 | VAR_246 | VAR_247
      | VAR_248 | VAR_249 | VAR_250 | VAR_251 | VAR_252 | VAR_253 | VAR_254 | VAR_255
      | EXT_0   | EXT_1   | EXT_2   | EXT_3   | EXT_4   | EXT_5   | EXT_6   | EXT_7
      | EXT_8   | EXT_9   | EXT_10  | EXT_11  | EXT_12  | EXT_13  | EXT_14
      | EXT_16  | EXT_17  | EXT_18  | EXT_19  | EXT_20  | EXT_21  | EXT_22  | EXT_23
      | EXT_24  | EXT_25  | EXT_26  | EXT_27  | EXT_28  | EXT_29
      | ILLEGAL
