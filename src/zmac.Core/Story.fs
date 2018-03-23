namespace Zmac.Core

open Type
open Utility

module Story =

    let make source =
        let buffer = Memory.Static.make source
        let len = Memory.Static.length buffer
        if len >= Model.Header.HeaderSize then
            let staticMemoryOffset = 
                let hi = Memory.Static.read buffer (getHiByteAddress Model.Header.StaticMemoryPointer)
                let lo = Memory.Static.read buffer (getLoByteAddress Model.Header.StaticMemoryPointer)
                bytesToWord (hi, lo)
            if (staticMemoryOffset <= len) then
                let dynamicMemory, staticMemory = Memory.Static.split buffer (decrementByteAddress (ByteAddress staticMemoryOffset))
                Model.make (Memory.Dynamic.make dynamicMemory) staticMemory
            else
                failwithf "Invalid memory image: inconsistent static memory offset"
        else
            failwithf "Invalid memory image: incomplete header"
               
    let ofFile path =
        path 
        |> System.IO.File.ReadAllBytes
        |> make
