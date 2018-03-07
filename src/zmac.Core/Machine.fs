namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Machine =
    
    type T = { dynamicMemory: Memory.T; staticMemory: byte array }

    /// This module defines the structure of the header
    module Header = 
        [<Literal>]
        let HeaderSize                              = 64

        let VersionAddress                          = ByteAddress 0x00
        let HighMemoryPointer                       = WordAddress 0x04
        let DictionaryPointer                       = WordAddress 0x08  
        let ObjectTablePointer                      = WordAddress 0x0A  
        let GlobalVariablesTablePointer             = WordAddress 0x0C
        let StaticMemoryPointer                     = WordAddress 0x0E
        let AbbreviationsTablePointer               = WordAddress 0x18    

    open Header

    let readByte machine address =
        let sizeOfDynamic = (Memory.getLength machine.dynamicMemory)
        if (isAddressInRange address sizeOfDynamic) then
            Memory.readByte machine.dynamicMemory address
        else
            dereferenceByte machine.staticMemory (decrementByteAddressBy sizeOfDynamic address)
        |> int

    let readWord machine address =
        let hi = readByte machine (getHiByteAddress address) 
        let lo = readByte machine (getLoByteAddress address) 
        bytesToWord (byte hi, byte lo)

    let writeByte machine address value =
        // Any attempt to write beyond dynamic memory will fail on this call
        { machine with dynamicMemory = Memory.writeByte machine.dynamicMemory address (byte value) }

    let writeBit machine address bitNumber value =
        let original = readByte machine address
        let modified = writeBit bitNumber original value
        writeByte machine address modified

    let writeWord machine address value =
        let hi = (value >>> 8) &&& 0xFF
        let lo = value &&& 0xFF
        let machine' = writeByte machine (getHiByteAddress address) hi
        writeByte machine' (getLoByteAddress address) lo

    let version machine = 
        let version = readByte machine VersionAddress
        match version with
        | 1 -> Version1
        | 2 -> Version2
        | 3 -> Version3
        | 4 -> Version4
        | 5 -> Version5
        | 6 -> Version6
        | _ -> failwithf "Unrecognized machine version: %d" version

    let dictionaryAddress machine = 
        DictionaryAddress (readWord machine DictionaryPointer)

    let objectTableAddress machine = 
        ObjectTableAddress (readWord machine ObjectTablePointer)

    let staticMemoryAddress machine = 
        StaticMemoryAddress (readWord machine StaticMemoryPointer)

    let abbreviationsTableAddress machine = 
        AbbreviationsTableAddress (readWord machine AbbreviationsTablePointer)

    let globalVariablesTableAddress machine = 
        GlobalVariablesTableAddress (readWord machine GlobalVariablesTablePointer)

    let isVersion4OrLater machine =
        match (version machine) with
        | Version1 | Version2 | Version3 -> false
        | _ -> true

    let create dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }

    module Helpers = 

        let createFromBytes bs =
            let len = Array.length bs
            if len >= HeaderSize then
                let staticMemoryBase = 
                    let hi = dereferenceByte bs (getHiByteAddress StaticMemoryPointer)
                    let lo = dereferenceByte bs (getLoByteAddress StaticMemoryPointer)
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
