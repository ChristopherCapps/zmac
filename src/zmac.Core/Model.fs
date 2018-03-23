namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Model =
    
    type T = { 
        dynamicMemory: Memory.Dynamic.T
        staticMemory: Memory.Static.T
    }

    let readByte model address =
        let sizeOfDynamic = Memory.Dynamic.length model.dynamicMemory
        if (isAddressInRange address sizeOfDynamic) then
            Memory.Dynamic.read model.dynamicMemory address
        else
            Memory.Static.read model.staticMemory (decrementByteAddressBy sizeOfDynamic address)
        |> int

    let readWord model address =
        let hi = readByte model (getHiByteAddress address) 
        let lo = readByte model (getLoByteAddress address) 
        bytesToWord (byte hi, byte lo)

    let writeByte model address value =
        // Any attempt to write beyond dynamic memory will fail on this call
        { model with dynamicMemory = Memory.Dynamic.write model.dynamicMemory address (byte value) }

    let writeBit model address bitNumber value =
        let original = readByte model address
        let modified = writeBit bitNumber original value
        writeByte model address modified

    let writeWord model address value =
        let hi = (value >>> 8) &&& 0xFF
        let lo = value &&& 0xFF
        let model' = writeByte model (getHiByteAddress address) hi
        writeByte model' (getLoByteAddress address) lo

    /// This module defines the structure of the header
    module Header = 
        [<Literal>]
        let HeaderSize                              = 64
        
        /// Note: A "pointer" is an address where another address is stored

        (*
            === Versions 3 and below only follow ===
        *)

        /// The address containing the value of the Z-model specification to which this model complies
        let VersionAddress                          = ByteAddress 0x00
        /// A pointer to the address representing the start of "high" memory
        let HighMemoryPointer                       = WordAddress 0x04
        /// A pointer to the address representing the start of the dictionary table
        let DictionaryPointer                       = WordAddress 0x08
        /// A pointer to the address representing the start of the object table
        let ObjectTablePointer                      = WordAddress 0x0A
        /// A pointer to the address representing the start of the global variables table
        let GlobalVariablesTablePointer             = WordAddress 0x0C
        /// A pointer to the address representing the start of "static" memory
        let StaticMemoryPointer                     = WordAddress 0x0E
        /// A pointer to the address representing the start of the abbreviations table
        let AbbreviationsTablePointer               = WordAddress 0x18
        /// In versions before 6, the initial program instruction byte address; in V6,
        /// a packed address of the "main" routine
        let InitialProgramCounterAddress            = WordAddress 0x06
        /// Conventional: Release number (2 bytes)
        let ReleaseNumberAddress                    = WordAddress 0x02
        /// Conventional: Serial number (6 ASCII chars)
        let SerialNumberAddress                     = ByteAddress 0x12
        /// Standard revision number
        let StandardRevisionNumberAddress           = ByteAddress 0x32

        /// Flags1
        /// Base of high memory
        /// Flags2
        /// File length
        /// File checksum

    (*
        === Versions 3 and below only follow ===
    *)

    let version model = 
        let version = readByte model Header.VersionAddress
        match version with
        | 1 -> Version1
        | 2 -> Version2
        | 3 -> Version3
        | 4 -> Version4
        | 5 -> Version5
        | 6 -> Version6
        | _ -> failwithf "Unrecognized model version: %d" version

    let isVersion4OrLater model =
        (version model) >= Version4

    let dictionaryAddress model = 
        DictionaryAddress (readWord model Header.DictionaryPointer)

    let objectTableAddress model = 
        ObjectTableAddress (readWord model Header.ObjectTablePointer)

    let staticMemoryAddress model = 
        StaticMemoryAddress (readWord model Header.StaticMemoryPointer)

    let abbreviationsTableAddress model = 
        AbbreviationsTableAddress (readWord model Header.AbbreviationsTablePointer)

    let globalVariablesTableAddress model = 
        GlobalVariablesTableAddress (readWord model Header.GlobalVariablesTablePointer)

    let initialProgramCounter model =
        let ipc = readWord model Header.InitialProgramCounterAddress
        match (version model) with
        | Version6 -> InstructionAddress (ipc*4+1)
        | _ -> InstructionAddress ipc

    let serialNumber model =
        [|0..5|]
        |> Array.map (fun i ->
            readByte model (incrementByteAddressBy i Header.SerialNumberAddress)
            |> intToChar)
        |> charSeqToString
        |> SerialNumber

    let releaseNumber model =
        ReleaseNumber (readWord model Header.ReleaseNumberAddress)

    (*
        === Factory function and helpers ===
    *)

    let make dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }
