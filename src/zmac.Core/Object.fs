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

    let objectPropertyDefaultAddress machine (ObjectProperty property) =
        //let (ObjectPropertyNumber n') = n    
        if property < 1 || property > (objectPropertyCount machine) then
            failwithf "Object property number out of range: %d. The valid range is %d to %d." property 1 (objectPropertyCount machine)
        let (ObjectPropertyDefaultsAddress tableAddress) = objectPropertyDefaultsAddress machine
        let (WordAddress address) = incrementWordAddressBy (property-1) (WordAddress tableAddress)
        ObjectPropertyAddress address

    let objectPropertyDefault machine property =
        let (ObjectPropertyAddress address) = objectPropertyDefaultAddress machine property
        readWord machine (WordAddress address)

    let objectTreeAddress machine = 
        let (ObjectTableAddress tableAddress) = objectTableAddress machine
        let (WordAddress address) = incrementWordAddressBy (objectPropertyCount machine) (WordAddress tableAddress)
        ObjectTreeAddress address

    let objectAddress machine (Object obj) =
        if obj < 1 || obj > (objectCount machine) then
            failwithf "Object number out of range: %d. The valid range is %d to %d." obj 1 (objectCount machine)
        let (ObjectTreeAddress treeAddress) = objectTreeAddress machine
        let (ByteAddress address) = (incrementByteAddressBy ((obj-1)*(objectEntrySizeBytes machine)) (ByteAddress treeAddress))
        ObjectAddress address

    let objectAttributesAddress machine obj =
        let (ObjectAddress address) = objectAddress machine obj
        ObjectAttributesAddress address

    let objectAttributeAddress machine obj (ObjectAttribute attribute) = 
        if attribute < 0 || attribute > ((objectAttributeCount machine) - 1) then
            failwithf "Object attribute number out of range: %d. The valid range is %d to %d." attribute 0 (objectAttributeCount machine)
        let (ObjectAttributesAddress address) = objectAttributesAddress machine obj
        let (ByteAddress attributeAddress) = incrementByteAddressBy (attribute / 8)  (ByteAddress address)
        let bitNumber = BitNumber (7 - (attribute % 8))
        ObjectAttributeAddress (attributeAddress, bitNumber)

    let readObjectAttribute machine obj attribute =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress machine obj attribute
        readBit bitNumber (readByte machine (ByteAddress address))

    let isObjectAttributeSet = readObjectAttribute

    let writeObjectAttribute machine obj attribute isSet =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress machine obj attribute
        writeBit machine (ByteAddress address) bitNumber isSet

    let setObjectAttribute machine obj attribute = 
        writeObjectAttribute machine obj attribute true

    let clearObjectAttribute machine obj attribute = 
        writeObjectAttribute machine obj attribute false

    let objectAttributeList machine obj =
        [|0..(objectAttributeCount machine)-1|]
        |> Array.map (ObjectAttribute >> (readObjectAttribute machine obj))

    let readObjectNumber machine (ObjectNumberAddress address) =
        if (isVersion4OrLater machine) then
            readWord machine (WordAddress address)
        else
            readByte machine (ByteAddress address)
        |> Object

    let writeObjectNumber machine (ObjectNumberAddress address) (Object obj) =
        if (isVersion4OrLater machine) then
            writeWord machine (WordAddress address) obj
        else
            writeByte machine (ByteAddress address) obj

    let objectParentAddress machine obj =
        let (ObjectAttributesAddress address) = objectAttributesAddress machine obj
        ObjectNumberAddress (address + (objectAttributeSizeBytes machine))

    let objectSiblingAddress machine obj =
        let (ObjectNumberAddress parent) = objectParentAddress machine obj
        ObjectNumberAddress (parent + (objectNumberSizeBytes machine))

    let objectChildAddress machine obj =
        let (ObjectNumberAddress sibling) = objectSiblingAddress machine obj
        ObjectNumberAddress (sibling + (objectNumberSizeBytes machine))

    let private readObjectRelation faddress machine obj =
        readObjectNumber machine (faddress machine obj)

    let private writeObjectRelation faddress machine obj relation =
        writeObjectNumber machine (faddress machine obj) relation

    let readObjectParent = 
        readObjectRelation objectParentAddress

    let readObjectSibling = 
        readObjectRelation objectSiblingAddress

    let readObjectChild = 
        readObjectRelation objectChildAddress

    let objectPropertiesPointer machine obj =
        let (ObjectNumberAddress child) = objectChildAddress machine obj
        WordAddress (child + objectNumberSizeBytes machine)

    let objectPropertiesAddress machine obj =
        ByteAddress (readWord machine (objectPropertiesPointer machine obj))

    let objectShortName machine obj =
        let (ByteAddress address) = objectPropertiesAddress machine obj
        Text.readZString machine (ZStringAddress (address+1))