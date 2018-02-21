namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Machine =
    
    type T = { dynamicMemory: Memory.T; staticMemory: byte array }

    [<Literal>]
    let HeaderSize                              = 64

    let VersionAddress                          = ByteAddress 0x00
    let HighMemoryPointer                       = WordAddress 0x04
    let StaticMemoryPointer                     = WordAddress 0x0e
    let DictionaryPointer                       = WordAddress 0x08
    let AbbreviationsTablePointer               = WordAddress 0x18

    let readByte machine address =
        let sizeOfDynamic = (Memory.getLength machine.dynamicMemory)
        if (isAddressInRange address sizeOfDynamic) then
            Memory.readByte machine.dynamicMemory address
        else
            dereferenceByte machine.staticMemory (decrementByteAddressBy sizeOfDynamic address)

    let readWord machine address =
        let hi = readByte machine (getHiByteAddress address) 
        let lo = readByte machine (getLoByteAddress address) 
        bytesToWord (hi, lo)

    let writeByte machine address value =
        // Any attempt to write beyond dynamic memory will fail on this call
        { machine with dynamicMemory = Memory.writeByte machine.dynamicMemory address value }

    let dictionaryAddress machine = DictionaryAddress (readWord machine DictionaryPointer)
    let staticMemoryAddress machine = StaticMemoryAddress (readWord machine StaticMemoryPointer)
    let abbreviationsTableAddress machine = AbbreviationsTableAddress (readWord machine AbbreviationsTablePointer)

    let create dynamicMemory staticMemory =
        { dynamicMemory = dynamicMemory; staticMemory = staticMemory }

    let readVersion machine = 
        let version = readByte machine VersionAddress
        match version with
        | 1uy -> Version1
        | 2uy -> Version2
        | 3uy -> Version3
        | 4uy -> Version4
        | 5uy -> Version5
        | 6uy -> Version6
        | _ -> failwithf "Unrecognized machine version: %d" version

    let isVersion4OrLater machine =
        match (readVersion machine) with
        | Version1 | Version2 | Version3 -> false
        | _ -> true

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


//    module Version = 

//        type T = Version1 | Version2 | Version3 | Version4 | Version5 | Version6
        
//        let identify id = 
//            match id with
//            | 1uy -> Version1
//            | 2uy -> Version2
//            | 3uy -> Version3
//            | 4uy | 5uy | 6uy -> failwithf "Unsupported machine version: %d" id
//            | _ -> failwithf "Unrecognized machine version: %d" id
            
//    type T = { version: Version.T; memory: Memory.T }

//    //let readByte address machine = ZMemory.readByte address machine.memory

//    module ZHeader =
        
//        [<Literal>]
//        let Length = 64

//        let ZAddress_Version            = ZAddress 0
//        let ZAddress_Flags1             = ZAddress 1
//        let ZAddress_HighMemoryBase     = ZAddress 0x04
//        let ZAddress_InitialPC          = ZAddress 0x06 // V1-5
//        let ZAddress_MainRoutine        = ZAddress 0x06 // V6
//        let ZAddress_DictionaryAddress  = ZAddress 0x08
        
//        let readVersion             = readByte ZAddress_Version
//        let readFlags1              = readByte ZAddress_Flags1
//        let readHighMemoryBase      = readByte ZAddress_HighMemoryBase
//        let readInitialPC           = readByte ZAddress_InitialPC
//        let readDictionaryAddress   = readByte ZAddress_DictionaryAddress
        
//    open ZHeader

//    type StatusLineType = ScoreTurns | HoursMins

//    let readStatusLineType machine = 
//        machine.memory
//        |> readFlags1 
//        |> translateBit 1 (HoursMins, ScoreTurns)

//    let readDictionary machine =
//        let dictionaryAddress = machine.memory |> readWordAsInt ZAddress_DictionaryAddress |> ZAddress
//        let kbdInputCodesCount = machine.memory |> readByteAsInt dictionaryAddress
//        let kbdInputCodes = machine.memory |> readBytes (dictionaryAddress.+.1) kbdInputCodesCount
//        let entryLength = machine.memory |> readByte (dictionaryAddress.+.(kbdInputCodesCount+1))
//        let entryCount = machine.memory |> readWord (dictionaryAddress.+.(kbdInputCodesCount+2))
//        printfn "Each dictionary entry is %d bytes and there are %d entries." entryLength entryCount

//    let create memory = 
//        // should we make the validations a generic type and then iterate through them?
//        let length = length memory
//        if (length < ZHeader.Length) then 
//            failwithf "Machine memory size (%d) is smaller than minimum requirement of %d" length ZHeader.Length
//        { 
//          version = Version.identify (readVersion memory)
//          memory = memory 
//        }
  
//    let run machine =
//        printfn "The running machine implements %A of the specification.\nStatus line type is %A." (machine.version) (readStatusLineType machine)
//        machine |> readDictionary
//        0
