namespace Zmac.Core

open Type
open Utility
open Story

module Object =

    (* Objects are numbered from 1 *)
    (* Properties are numbered from 1 *)
    (* Attributes are numbered from 0 *)
    
    let objectMaxCount story =
        if (isVersion4OrLater story) then 0xFFFF else 0xFF

    let objectPropertyCount story =
        if (isVersion4OrLater story) then 63 else 31

    let objectAttributeCount story =
        if (isVersion4OrLater story) then 48 else 32

    let allObjectAttributes story =
        let lastAttribute = (objectAttributeCount story)-1
        [|0..lastAttribute|]
        |> Array.map ObjectAttribute

    let objectAttributeSizeBytes story =
        if (isVersion4OrLater story) then 6 else 4

    let objectNumberSizeBytes story =
        if (isVersion4OrLater story) then 2 else 1

    let objectEntrySizeBytes story =
        if (isVersion4OrLater story) then 14 else 9

    let objectPropertyDefaultsAddress story = 
        let (ObjectTableAddress address) = objectTableAddress story
        ObjectPropertyDefaultsAddress address

    let objectPropertyDefaultAddress story (ObjectProperty property) =
        //let (ObjectPropertyNumber n') = n    
        if property < 1 || property > (objectPropertyCount story) then
            failwithf "Object property number out of range: %d. The valid range is %d to %d." property 1 (objectPropertyCount story)
        let (ObjectPropertyDefaultsAddress tableAddress) = objectPropertyDefaultsAddress story
        let (WordAddress address) = incrementWordAddressBy (property-1) (WordAddress tableAddress)
        ObjectPropertyAddress address

    let objectPropertyDefault story property =
        let (ObjectPropertyAddress address) = objectPropertyDefaultAddress story property
        readWord story (WordAddress address)

    let objectTreeAddress story = 
        let (ObjectTableAddress tableAddress) = objectTableAddress story
        let (WordAddress address) = incrementWordAddressBy (objectPropertyCount story) (WordAddress tableAddress)
        ObjectTreeAddress address

    (* 
       Guesses the number of objects by finding the lowest address of object properties and assuming that
       all object tree definitions are below that address. Given the start of the object tree and the
       size of each object entry, determine the object count. 
    *)
    let objectCount story =
        let offset = objectEntrySizeBytes story
        let rec loop lowPropAddr nextPropAddrPtr (nextObjNum, nextObjAddr) =
            let nextPropAddr = readWord story (WordAddress nextPropAddrPtr)
            let lowPropAddr' = System.Math.Min (lowPropAddr, nextPropAddr)
            if nextObjAddr >= lowPropAddr' then nextObjNum-1 else 
                loop lowPropAddr' (nextPropAddrPtr+offset) (nextObjNum+1, nextObjAddr+offset)
        let (ObjectTreeAddress object1Address) = objectTreeAddress story        
        let obj1PropertiesPtr = object1Address + (objectEntrySizeBytes story) - WordLength
        loop 0xFFFF obj1PropertiesPtr (2, obj1PropertiesPtr+WordLength)

    let allObjects story =
        let count = objectCount story
        [|1..count|] 
        |> Array.map Object

    let objectAddress story (Object obj) =
        if obj < 1 || obj > (objectCount story) then
            failwithf "Object number out of range: %d. The valid range is %d to %d." obj 1 (objectCount story)
        let (ObjectTreeAddress treeAddress) = objectTreeAddress story
        let (ByteAddress address) = (incrementByteAddressBy ((obj-1)*(objectEntrySizeBytes story)) (ByteAddress treeAddress))
        ObjectAddress address

    let objectAttributesAddress story obj =
        let (ObjectAddress address) = objectAddress story obj
        ObjectAttributesAddress address

    let objectAttributeAddress story obj (ObjectAttribute attribute) = 
        if attribute < 0 || attribute > ((objectAttributeCount story) - 1) then
            failwithf "Object attribute number out of range: %d. The valid range is %d to %d." attribute 0 (objectAttributeCount story)
        let (ObjectAttributesAddress address) = objectAttributesAddress story obj
        let (ByteAddress attributeAddress) = incrementByteAddressBy (attribute / 8)  (ByteAddress address)
        let bitNumber = BitNumber (7 - (attribute % 8))
        ObjectAttributeAddress (attributeAddress, bitNumber)

    let readObjectAttribute story obj attribute =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress story obj attribute
        readBit bitNumber (readByte story (ByteAddress address))

    let isObjectAttributeSet = readObjectAttribute

    let writeObjectAttribute story obj attribute isSet =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress story obj attribute
        writeBit story (ByteAddress address) bitNumber isSet

    let setObjectAttribute story obj attribute = 
        writeObjectAttribute story obj attribute true

    let clearObjectAttribute story obj attribute = 
        writeObjectAttribute story obj attribute false

    let readAllObjectAttributes story obj =
        story
        |> allObjectAttributes
        |> Array.map (readObjectAttribute story obj)

    let readObjectNumber story (ObjectNumberAddress address) =
        if (isVersion4OrLater story) then
            readWord story (WordAddress address)
        else
            readByte story (ByteAddress address)
        |> Object

    let writeObjectNumber story (ObjectNumberAddress address) (Object obj) =
        if (isVersion4OrLater story) then
            writeWord story (WordAddress address) obj
        else
            writeByte story (ByteAddress address) obj

    let objectParentAddress story obj =
        let (ObjectAttributesAddress address) = objectAttributesAddress story obj
        ObjectNumberAddress (address + (objectAttributeSizeBytes story))

    let objectSiblingAddress story obj =
        let (ObjectNumberAddress parent) = objectParentAddress story obj
        ObjectNumberAddress (parent + (objectNumberSizeBytes story))

    let objectChildAddress story obj =
        let (ObjectNumberAddress sibling) = objectSiblingAddress story obj
        ObjectNumberAddress (sibling + (objectNumberSizeBytes story))

    let private readObjectRelation faddress story obj =
        readObjectNumber story (faddress story obj)

    let private writeObjectRelation faddress story obj relation =
        writeObjectNumber story (faddress story obj) relation

    let readObjectParent = 
        readObjectRelation objectParentAddress

    let readObjectSibling = 
        readObjectRelation objectSiblingAddress

    let readObjectChild = 
        readObjectRelation objectChildAddress

    let objectPropertiesPointer story obj =
        let (ObjectNumberAddress child) = objectChildAddress story obj
        WordAddress (child + objectNumberSizeBytes story)

    let objectPropertiesAddress story obj =
        ByteAddress (readWord story (objectPropertiesPointer story obj))

    let objectShortNameAddress = objectPropertiesAddress

    let objectPropertiesDataAddress story obj =
        let (ByteAddress address) = objectPropertiesAddress story obj
        let shortNameLengthWords = readByte story (ByteAddress address)
        ObjectPropertiesDataAddress (address + 1 + shortNameLengthWords*WordLength)

    let readObjectProperties story obj =
        let rec loop propAddr acc =
            let propAddr' = ByteAddress propAddr
            let size = readByte story propAddr'
            if size = 0 then acc else
            if (isVersion4OrLater story) then
                let propNum = readBits BitNumber5 BitCount6 size // Bottom 6 bits
                if (readBit BitNumber7 size) then                    
                    let dataSize = readBits BitNumber5 BitCount6 (readByte story (incrementByteAddress propAddr'))
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
                let dataSize =  (size / 32 + 1) // Same as top 3 bits, plus 1
                let nextPropAddr = propAddr + 1 + dataSize
                loop nextPropAddr (acc@[(ObjectPropertyData (ObjectProperty (size % 32), // Same as bottom 5 bits 
                                                             ObjectPropertyDataSize dataSize, 
                                                             ObjectPropertyDataAddress (propAddr + 1)))])
                                                                 
        let (ObjectPropertiesDataAddress address) = objectPropertiesDataAddress story obj
        loop address []

    let objectShortName story obj =
        let (ByteAddress address) = objectShortNameAddress story obj
        Text.readZString story (ZStringAddress (address+1))

    let objectToString story obj =
        let (Object n) = obj
        let shortName = objectShortName story obj
        let (Object parent), (Object sibling), (Object child) = 
            readObjectParent story obj,
            readObjectSibling story obj,
            readObjectChild story obj
        let attributes = 
            story
            |> allObjectAttributes
            |> Array.choose (fun attr -> if (isObjectAttributeSet story obj attr) then Some attr else None)
            |> Array.map (fun (ObjectAttribute i) -> i)
         
        sprintf "%5d. %-35s[Parent: %5d] [Sibling: %5d] [Child: %5d] [Attributes: %A]" 
            n shortName parent sibling child attributes

    let showObject story obj = 
        printfn "%s" (objectToString story obj)
        
    let showObjects story =
        story
        |> allObjects
        |> Array.map (objectToString story)
        |> Array.iter (printfn "%s")
