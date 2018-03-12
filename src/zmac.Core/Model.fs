namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Model =
    
    type T = { dynamicMemory: Memory.Dynamic.T; staticMemory: Memory.Static.T }

    /// This module defines the structure of the header
    module Header = 
        [<Literal>]
        let HeaderSize                              = 64

        /// The address containing the value of the Z-model specification to which this story complies
        let VersionAddress                          = ByteAddress 0x00
        
        /// A "pointer" is an address where another address is stored

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

    open Header

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

    let version model = 
        let version = readByte model VersionAddress
        match version with
        | 1 -> Version1
        | 2 -> Version2
        | 3 -> Version3
        | 4 -> Version4
        | 5 -> Version5
        | 6 -> Version6
        | _ -> failwithf "Unrecognized model version: %d" version

    let dictionaryAddress model = 
        DictionaryAddress (readWord model DictionaryPointer)

    let objectTableAddress model = 
        ObjectTableAddress (readWord model ObjectTablePointer)

    let staticMemoryAddress model = 
        StaticMemoryAddress (readWord model StaticMemoryPointer)

    let abbreviationsTableAddress model = 
        AbbreviationsTableAddress (readWord model AbbreviationsTablePointer)

    let globalVariablesTableAddress model = 
        GlobalVariablesTableAddress (readWord model GlobalVariablesTablePointer)

    let isVersion4OrLater model =
        match (version model) with
        | Version1 | Version2 | Version3 -> false
        | _ -> true

    let make dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }

    module Helpers = 

        let createFromBytes source =
            let buffer = Memory.Static.make source
            let len = Memory.Static.length buffer
            if len >= HeaderSize then
                let staticMemoryBase = 
                    let hi = Memory.Static.read buffer (getHiByteAddress StaticMemoryPointer)
                    let lo = Memory.Static.read buffer (getLoByteAddress StaticMemoryPointer)
                    bytesToWord (hi, lo)
                if (staticMemoryBase <= len) then
                    let dynamicMemory', staticMemory' = subdivideBytes bs (ByteAddress (staticMemoryBase-1))
                    { staticMemory = staticMemory'; dynamicMemory = Memory.make dynamicMemory' }
                else
                    failwithf "Invalid story image: inconsistent static memory offset"
            else
                failwithf "Invalid story image: incomplete header"
                   
        let createFromFile path =
            path 
            |> System.IO.File.ReadAllBytes
            |> createFromBytes
