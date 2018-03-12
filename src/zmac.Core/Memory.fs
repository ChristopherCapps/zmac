namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Memory = 
    
    module Static =
        type T = T of byte array

        let make source = 
            T (Array.copy source)

        let read (T buffer) address =
            if (isAddressInRange address (Array.length buffer)) then
                let (ByteAddress address') = address
                buffer.[address']
            else
                failwithf "%A is out of range for the buffer given" address

        let length (T buffer) =
            Array.length buffer

        let range source loAddress hiAddress =
            let len = length source
            if (isAddressInRange loAddress len) then
                let hiAddress' = 
                    match hiAddress with
                    | Some (ByteAddress hiAddress') -> 
                        if (isAddressInRange hiAddress len) then hiAddress'
                        else failfwithf "%A is out of range for the buffer given" hiAddress
                    | None -> 
                        len-1
                let (T buffer) = source

                

        /// Divides the given static memory into two blocks at the specified address, with the first block containing the address.
        let subdivide ((T buffer) as source) ((ByteAddress address) as address') =
            if isAddressInRange address ((length source) - 1) then
                let (ByteAddress address') = address
                let (T buffer) = source
                (make buffer.[..address']), (make buffer.[address'+1..])
            else
                failwithf "%A is out of range for the buffer given" address   

    module Dynamic = 

        module Updates =
            type T = T of Map<int,byte>

            let make = T Map.empty

            let empty = make

            let read (T updates) (ByteAddress address) =
                updates 
                |> Map.tryFind address  

            let write (T updates) (ByteAddress address) value =
                updates 
                |> Map.add address value 
                |> T

        type T = { source: Static.T; updates: Updates.T }

        let make source =
            { source = Static.make source; updates = Updates.make }

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
