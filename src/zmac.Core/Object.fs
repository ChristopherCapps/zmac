namespace Zmac.Core

open Type
open Utility
open Machine

module Object =

    (* Objects are numbered from 1 *)
    (* Properties are numbered from 1 *)
    (* Attributes are numbered from 0 *)
    
    let objectMaxCount machine =
        if (isVersion4OrLater machine) then 0xFFFF else 0xFF

    let objectPropertyCount machine =
        if (isVersion4OrLater machine) then 63 else 31

    let objectAttributeCount machine =
        if (isVersion4OrLater machine) then 48 else 32

    let objectAttributeRange machine =
        seq { 0..(objectAttributeCount machine)-1 }

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

    (* 
       Guesses the number of objects by finding the lowest address of object properties and assuming that
       all object tree definitions are below that address. Given the start of the object tree and the
       size of each object entry, determine the object count. 
    *)
    let objectCount machine =
        let offset = objectEntrySizeBytes machine
        let rec loop lowPropAddr nextPropAddrPtr (nextObjNum, nextObjAddr) =
            let nextPropAddr = readWord machine (WordAddress nextPropAddrPtr)
            let lowPropAddr' = System.Math.Min (lowPropAddr, nextPropAddr)
            if nextObjAddr >= lowPropAddr' then nextObjNum-1 else 
                loop lowPropAddr' (nextPropAddrPtr+offset) (nextObjNum+1, nextObjAddr+offset)
        let (ObjectTreeAddress object1Address) = objectTreeAddress machine        
        let obj1PropertiesPtr = object1Address + (objectEntrySizeBytes machine) - WordLength
        loop 0xFFFF obj1PropertiesPtr (2, obj1PropertiesPtr+WordLength)

    let allObjects machine =
        [1..(objectCount machine)] |> Seq.map Object

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
        machine
        |> objectAttributeRange
        |> Seq.map (ObjectAttribute >> (readObjectAttribute machine obj))

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

    let objectShortNameAddress = objectPropertiesAddress

    let objectPropertiesDataAddress machine obj =
        let (ByteAddress address) = objectPropertiesAddress machine obj
        let shortNameLengthWords = readByte machine (ByteAddress address)
        ObjectPropertiesDataAddress (address + 1 + shortNameLengthWords*WordLength)

    let readObjectProperties machine obj =
        let rec loop propAddr acc =
            let size = readByte machine (ByteAddress propAddr)
            if size = 0 then acc else
            if (isVersion4OrLater machine) then
                let propNum = readBits BitNumber0 BitCount6 size
                if (readBit BitNumber7 size) then                    
                    let dataSize = readBits BitNumber0 BitCount6 (readByte machine (ByteAddress (propAddr+1)))
                    let dataSize' = if dataSize = 0 then 64 else dataSize
                    let nextPropAddr = propAddr + 2 + dataSize'
                    loop nextPropAddr (acc@[(ObjectPropertyData (ObjectProperty propNum,
                                                                 ObjectPropertyDataSize dataSize',
                                                                 ObjectPropertyDataAddress (propAddr + 2)))])
                else
                    let dataSize = if (readBit BitNumber6 size) then 2 else 1
                    let nextPropAddr = propAddr + 1 + dataSize
                    loop nextPropAddr (acc@[(ObjectPropertyData (ObjectProperty propNum,
                                                                 ObjectPropertyDataSize dataSize,
                                                                 ObjectPropertyDataAddress (propAddr + 1)))])                    
            else                
                let dataSize =  (size / 32 + 1)
                let nextPropAddr = propAddr + 1 + dataSize
                loop nextPropAddr (acc@[(ObjectPropertyData (ObjectProperty (size % 32), 
                                                             ObjectPropertyDataSize dataSize, 
                                                             ObjectPropertyDataAddress (propAddr + 1)))])
                                                                 
        let (ObjectPropertiesDataAddress address) = objectPropertiesDataAddress machine obj
        loop address []

    let objectShortName machine obj =
        let (ByteAddress address) = objectShortNameAddress machine obj
        Text.readZString machine (ZStringAddress (address+1))

    let objectToString machine obj =
        let (Object n) = obj
        let shortName = objectShortName machine obj
        let (Object parent), (Object sibling), (Object child) = 
            readObjectParent machine obj,
            readObjectSibling machine obj,
            readObjectChild machine obj
        let attributes = 
            machine
            |> objectAttributeRange
            |> Seq.choose (fun i -> if (isObjectAttributeSet machine obj (ObjectAttribute i)) then Some i else None)
         
        sprintf "%5d. %-35s[Parent: %5d] [Sibling: %5d] [Child: %5d] [Attributes: %A]" 
            n shortName parent sibling child attributes

    let showObject machine obj = 
        printfn "%s" (objectToString machine obj)
        
    let showObjects machine =
        machine
        |> allObjects
        |> Seq.map (fun obj -> objectToString machine obj)
        |> Seq.iter (printfn "%s")
