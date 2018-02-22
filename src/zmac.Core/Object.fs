namespace Zmac.Core

open Type
open Utility
open Machine

module Object =

    [<Literal>]
    let ObjectPropertyCountV3           = 31
    [<Literal>]
    let ObjectPropertyCountV4           = 63
    [<Literal>]
    let ObjectSizeV3                    = 9
    [<Literal>]
    let ObjectSizeV4                    = 14

    let objectPropertyCount machine =
        if (isVersion4OrLater machine) then ObjectPropertyCountV4 else ObjectPropertyCountV3

    let objectPropertyDefaultsAddress machine = 
        let (ObjectTableAddress address) = objectTableAddress machine
        ObjectPropertyDefaultsAddress address

    let objectPropertyDefaultAddress machine (ObjectProperty n) =
        //let (ObjectPropertyNumber n') = n    
        if n < 1 || n > (objectPropertyCount machine) then
            failwithf "Object property number out of range: %d. The valid range is %d to %d." n 1 (objectPropertyCount machine)
        else
            let (ObjectPropertyDefaultsAddress tableAddress) = objectPropertyDefaultsAddress machine
            let (WordAddress address) = incrementWordAddressBy (n-1) (WordAddress tableAddress)
            ObjectPropertyAddress address

    let objectPropertyDefault machine n =
        let (ObjectPropertyAddress address) = objectPropertyDefaultAddress machine n
        readWord machine (WordAddress address)

    let objectEntrySize machine =
        if (isVersion4OrLater machine) then ObjectSizeV4 else ObjectSizeV3

    let objectTreeAddress machine = 
        let (ObjectTableAddress tableAddress) = objectTableAddress machine
        let (WordAddress address) = incrementWordAddressBy (objectPropertyCount machine) (WordAddress tableAddress)
        ObjectTreeAddress address

    let objectAddress machine (Object n) =
        let (ObjectTreeAddress treeAddress) = objectTreeAddress machine
        let (ByteAddress address) = (incrementByteAddressBy ((n-1)*(objectEntrySize machine)) (ByteAddress treeAddress))
        ObjectAddress address