namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Story =
    
    type T = { 
        dynamicMemory: Memory.Dynamic.T
        staticMemory: Memory.Static.T
        //processor: Processor.T
    }

    let readByte story address =
        let sizeOfDynamic = Memory.Dynamic.length story.dynamicMemory
        if (isAddressInRange address sizeOfDynamic) then
            Memory.Dynamic.read story.dynamicMemory address
        else
            Memory.Static.read story.staticMemory (decrementByteAddressBy sizeOfDynamic address)
        |> int

    let readWord story address =
        let hi = readByte story (getHiByteAddress address) 
        let lo = readByte story (getLoByteAddress address) 
        bytesToWord (byte hi, byte lo)

    let writeByte story address value =
        // Any attempt to write beyond dynamic memory will fail on this call
        { story with dynamicMemory = Memory.Dynamic.write story.dynamicMemory address (byte value) }

    let writeBit story address bitNumber value =
        let original = readByte story address
        let modified = writeBit bitNumber original value
        writeByte story address modified

    let writeWord story address value =
        let hi = (value >>> 8) &&& 0xFF
        let lo = value &&& 0xFF
        let story' = writeByte story (getHiByteAddress address) hi
        writeByte story' (getLoByteAddress address) lo

    /// This module defines the structure of the header
    module Header = 
        [<Literal>]
        let HeaderSize                              = 64

        /// The address containing the value of the Z-story specification to which this story complies
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

    let version story = 
        let version = readByte story Header.VersionAddress
        match version with
        | 1 -> Version1
        | 2 -> Version2
        | 3 -> Version3
        | 4 -> Version4
        | 5 -> Version5
        | 6 -> Version6
        | _ -> failwithf "Unrecognized story version: %d" version

    let isVersion4OrLater story =
        match (version story) with
        | Version1 | Version2 | Version3 -> false
        | _ -> true

    let dictionaryAddress story = 
        DictionaryAddress (readWord story Header.DictionaryPointer)

    let objectTableAddress story = 
        ObjectTableAddress (readWord story Header.ObjectTablePointer)

    let staticMemoryAddress story = 
        StaticMemoryAddress (readWord story Header.StaticMemoryPointer)

    let abbreviationsTableAddress story = 
        AbbreviationsTableAddress (readWord story Header.AbbreviationsTablePointer)

    let globalVariablesTableAddress story = 
        GlobalVariablesTableAddress (readWord story Header.GlobalVariablesTablePointer)

    let make dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }

    module Helpers = 

        let createFromBytes source =
            let buffer = Memory.Static.make source
            let len = Memory.Static.length buffer
            if len >= Header.HeaderSize then
                let staticMemoryOffset = 
                    let hi = Memory.Static.read buffer (getHiByteAddress Header.StaticMemoryPointer)
                    let lo = Memory.Static.read buffer (getLoByteAddress Header.StaticMemoryPointer)
                    bytesToWord (hi, lo)
                if (staticMemoryOffset <= len) then
                    let dynamicMemory, staticMemory = Memory.Static.split buffer (decrementByteAddress (ByteAddress staticMemoryOffset))
                    make (Memory.Dynamic.make dynamicMemory) staticMemory
                else
                    failwithf "Invalid memory image: inconsistent static memory offset"
            else
                failwithf "Invalid memory image: incomplete header"
                   
        let createFromFile path =
            path 
            |> System.IO.File.ReadAllBytes
            |> createFromBytes
