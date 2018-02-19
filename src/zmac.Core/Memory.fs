namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Memory = 
    
    type private Mutations = Map<int,byte>

    type T = { source: byte array; mutations: Mutations }

    let make source =
        { source = Array.copy source; mutations = Map.empty }

    let private readMutation memory (ByteAddress address) =
        memory.mutations |> Map.tryFind address  

    let private writeMutation memory (ByteAddress address) value =
        memory.mutations|> Map.add address value 

    let getLength memory =
        Array.length memory.source
        
    let readByte memory address =    
        let length = getLength memory
        if isAddressInRange address length then
            match (readMutation memory address) with
            | Some zb -> zb
            | None -> dereferenceByte memory.source address
        else
            failwithf "%A is invalid for memory of length %d" address length

    let readWord memory address =
        let hiAddress, loAddress = (getHiByteAddress address), (getLoByteAddress address)
        let hi, lo = (readByte memory hiAddress), (readByte memory loAddress)
        bytesToWord (hi, lo)

    let writeByte memory address value =
        let length = getLength memory
        if isAddressInRange address length then
            match (readByte memory address) with
            | b when b = value -> memory
            | _ -> { memory with mutations = (writeMutation memory address value) }
        else
            failwithf "%A is invalid for memory of length %d" address length

    let reset memory = 
        { memory with mutations = Map.empty }
