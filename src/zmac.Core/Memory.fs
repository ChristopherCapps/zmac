namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Memory = 
    
    module Static =
        type T = T of byte array

        let make source = 
            T (Array.copy source)

        let length (T buffer) =
            Array.length buffer

        let isAddressInRange memory address =
            Zmac.Core.Utility.isAddressInRange address (length memory)

        let read memory address =
            if (isAddressInRange memory address) then
                let (ByteAddress offset) = address
                let (T buffer) = memory
                buffer.[offset]
            else
                failwithf "%A is out of range for the memory given" address

        let highestAddress memory =            
            let len = length memory
            if len = 0 then failwithf "The given memory has no content"
            ByteAddress (len - 1)

        let range memory lowAddress highAddress =
            if not (isAddressInRange memory lowAddress) then
                failwithf "Low address (%A) is out of range for the memory given" lowAddress
            if not (isAddressInRange memory highAddress) then
                failwithf "High address (%A) is out of range for the memory given" highAddress
            
            // TODO: This is nicely functional & uncluttered but not our most performant option, so consider a more direct approach
            byteAddressRange lowAddress highAddress
            |> Array.map (read memory)
            |> make

        /// Divides the given static memory into two blocks at the specified address, with the first block containing the address itself.
        let split memory splitAddress =
            let lowRange = range memory (ByteAddress 0) splitAddress
            let highRange = 
                match (highestAddress memory) with
                | highestAddress when splitAddress = highestAddress -> 
                    make Array.empty
                | highestAddress -> 
                    range memory (incrementByteAddress splitAddress) highestAddress
            lowRange, highRange

    module Dynamic = 

        module Updates =
            type T = T of Map<int,byte>

            let make map = T map

            let empty = make Map.empty

            let read (T updates) (ByteAddress address) =
                updates 
                |> Map.tryFind address  

            let write (T updates) (ByteAddress address) value =
                updates 
                |> Map.add address value 
                |> make

        type T = { source: Static.T; updates: Updates.T }

        let make source =
            { source = source; updates = Updates.empty }

        let length memory =
            Static.length memory.source
            
        let read memory address =    
            let length = length memory
            if isAddressInRange address length then
                match (Updates.read memory.updates address) with
                | Some b ->
                    b
                | None ->
                    Static.read memory.source address
            else
                failwithf "%A is invalid for memory of length %d" address length

        let write memory address value =
            let length = length memory
            if isAddressInRange address length then
                match (read memory address) with
                | b when b = value -> 
                    memory
                | _ -> 
                    { memory with updates = (Updates.write memory.updates address value) }
            else
                failwithf "%A is invalid for memory of length %d" address length

        let reset memory = 
            { memory with updates = Updates.empty }
