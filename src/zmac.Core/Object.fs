namespace Zmac.Core

open Type
open Utility
open Machine

module Object =

    (* Objects are numbered from 1 *)
    (* Properties are numbered from 1 *)
    (* Attributes are numbered from 0 *)
    
    let objectCount machine =
        if (isVersion4OrLater machine) then 0xFFFF else 0xFF

    let objectPropertyCount machine =
        if (isVersion4OrLater machine) then 63 else 31

    let objectAttributeCount machine =
        if (isVersion4OrLater machine) then 48 else 32

    let objectAttributeSizeBytes machine =
        if (isVersion4OrLater machine) then 6 else 4

    let objectNumberSizeBytes machine =
        if (isVersion4OrLater machine) then 2 else 1

    let objectEntrySizeBytes machine =
        if (isVersion4OrLater machine) then 14 else 9

    let objectPropertyDefaultsAddress machine = 
        let (ObjectTableAddress address) = objectTableAddress machine
        ObjectPropertyDefaultsAddress address

    let objectPropertyDefaultAddress machine (ObjectProperty n) =
        //let (ObjectPropertyNumber n') = n    
        if n < 1 || n > (objectPropertyCount machine) then
            failwithf "Object property number out of range: %d. The valid range is %d to %d." n 1 (objectPropertyCount machine)
        let (ObjectPropertyDefaultsAddress tableAddress) = objectPropertyDefaultsAddress machine
        let (WordAddress address) = incrementWordAddressBy (n-1) (WordAddress tableAddress)
        ObjectPropertyAddress address

    let objectPropertyDefault machine n =
        let (ObjectPropertyAddress address) = objectPropertyDefaultAddress machine n
        readWord machine (WordAddress address)

    let objectTreeAddress machine = 
        let (ObjectTableAddress tableAddress) = objectTableAddress machine
        let (WordAddress address) = incrementWordAddressBy (objectPropertyCount machine) (WordAddress tableAddress)
        ObjectTreeAddress address

    let objectAddress machine (Object n) =
        if n < 1 || n > (objectCount machine) then
            failwithf "Object number out of range: %d. The valid range is %d to %d." n 1 (objectCount machine)
        let (ObjectTreeAddress treeAddress) = objectTreeAddress machine
        let (ByteAddress address) = (incrementByteAddressBy ((n-1)*(objectEntrySizeBytes machine)) (ByteAddress treeAddress))
        ObjectAddress address

    let objectAttributesAddress machine n =
        let (ObjectAddress address) = objectAddress machine n
        ObjectAttributesAddress address

    let objectAttributeAddress machine n (ObjectAttribute attribute) = 
        if attribute < 0 || attribute > ((objectAttributeCount machine) - 1) then
            failwithf "Object attribute number out of range: %d. The valid range is %d to %d." attribute 0 (objectAttributeCount machine)
        let (ObjectAttributesAddress address) = objectAttributesAddress machine n
        let (ByteAddress attributeAddress) = incrementByteAddressBy (attribute / 8)  (ByteAddress address)
        let bitNumber = BitNumber (7 - (attribute % 8))
        ObjectAttributeAddress (attributeAddress, bitNumber)

    let readObjectAttribute machine n attribute =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress machine n attribute
        readBit bitNumber (byteToInt (readByte machine (ByteAddress address)))

    let writeObjectAttribute machine n attribute isSet =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress machine n attribute
        writeBit machine (ByteAddress address) bitNumber isSet

    let setObjectAttribute machine n attribute = 
        writeObjectAttribute machine n attribute true

    let clearObjectAttribute machine n attribute = 
        writeObjectAttribute machine n attribute false

    let objectAttributeList machine n =
        [|0..(objectAttributeCount machine)-1|]
        |> Array.map (ObjectAttribute >> (readObjectAttribute machine n))

    let readObjectNumber machine (ObjectNumberAddress address) =
        if (isVersion4OrLater machine) then
            readWord machine (WordAddress address)
        else
            readByte machine (ByteAddress address) |> byteToInt
        |> Object

    let objectParentAddress machine n =
        let (ObjectAttributesAddress address) = objectAttributesAddress machine n
        ObjectNumberAddress (address + (objectAttributeSizeBytes machine))

    let objectSiblingAddress machine n =
        let (ObjectNumberAddress parent) = objectParentAddress machine n
        ObjectNumberAddress (parent + (objectNumberSizeBytes machine))

    let objectChildAddress machine n =
        let (ObjectNumberAddress sibling) = objectSiblingAddress machine n
        ObjectNumberAddress (sibling + (objectNumberSizeBytes machine))

    let readObjectParent machine n =
        readObjectNumber machine (objectParentAddress machine n)

    let readObjectSibling machine n =
        readObjectNumber machine (objectSiblingAddress machine n)

    let readObjectChild machine n =
        readObjectNumber machine (objectChildAddress machine n)
