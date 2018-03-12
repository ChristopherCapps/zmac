namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Machine =
    
    type T = { 
        dynamicMemory: Memory.Dynamic.T
        staticMemory: Memory.Static.T
        //processor: Processor.T
    }

    module Memory = 
        
        let readByte machine address =
            let sizeOfDynamic = Memory.Dynamic.length machine.dynamicMemory
            if (isAddressInRange address sizeOfDynamic) then
                Memory.Dynamic.read machine.dynamicMemory address
            else
                Memory.Static.read machine.staticMemory (decrementByteAddressBy sizeOfDynamic address)
            |> int

        let readWord machine address =
            let hi = readByte machine (getHiByteAddress address) 
            let lo = readByte machine (getLoByteAddress address) 
            bytesToWord (byte hi, byte lo)

        let writeByte machine address value =
            // Any attempt to write beyond dynamic memory will fail on this call
            { machine with dynamicMemory = Memory.Dynamic.write machine.dynamicMemory address (byte value) }

        let writeBit machine address bitNumber value =
            let original = readByte machine address
            let modified = writeBit bitNumber original value
            writeByte machine address modified

        let writeWord machine address value =
            let hi = (value >>> 8) &&& 0xFF
            let lo = value &&& 0xFF
            let machine' = writeByte machine (getHiByteAddress address) hi
            writeByte machine' (getLoByteAddress address) lo

        /// This module defines the structure of the header
        module Map = 
            [<Literal>]
            let HeaderSize                              = 64

            /// The address containing the value of the Z-machine specification to which this story complies
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

    open Memory

    let version machine = 
        let version = readByte machine Map.VersionAddress
        match version with
        | 1 -> Version1
        | 2 -> Version2
        | 3 -> Version3
        | 4 -> Version4
        | 5 -> Version5
        | 6 -> Version6
        | _ -> failwithf "Unrecognized machine version: %d" version

    let isVersion4OrLater machine =
        match (version machine) with
        | Version1 | Version2 | Version3 -> false
        | _ -> true

    let dictionaryAddress machine = 
        DictionaryAddress (readWord machine Map.DictionaryPointer)

    let objectTableAddress machine = 
        ObjectTableAddress (readWord machine Map.ObjectTablePointer)

    let staticMemoryAddress machine = 
        StaticMemoryAddress (readWord machine Map.StaticMemoryPointer)

    let abbreviationsTableAddress machine = 
        AbbreviationsTableAddress (readWord machine Map.AbbreviationsTablePointer)

    let globalVariablesTableAddress machine = 
        GlobalVariablesTableAddress (readWord machine Map.GlobalVariablesTablePointer)

    let make dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }

    module Helpers = 

        let createFromBytes source =
            let buffer = Memory.Static.make source
            let len = Memory.Static.length buffer
            if len >= Map.HeaderSize then
                let staticMemoryOffset = 
                    let hi = Memory.Static.read buffer (getHiByteAddress Map.StaticMemoryPointer)
                    let lo = Memory.Static.read buffer (getLoByteAddress Map.StaticMemoryPointer)
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
