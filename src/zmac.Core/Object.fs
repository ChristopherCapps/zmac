namespace Zmac.Core

open Type
open Utility
open Model

module Object =

    (* Objects are numbered from 1 *)
    (* Properties are numbered from 1 *)
    (* Attributes are numbered from 0 *)
    
    let objectMaxCount model =
        if (isVersion4OrLater model) then 0xFFFF else 0xFF

    let objectPropertyCount model =
        if (isVersion4OrLater model) then 63 else 31

    let objectAttributeCount model =
        if (isVersion4OrLater model) then 48 else 32

    let allObjectAttributes model =
        let lastAttribute = (objectAttributeCount model)-1
        [|0..lastAttribute|]
        |> Array.map ObjectAttribute

    let objectAttributeSizeBytes model =
        if (isVersion4OrLater model) then 6 else 4

    let objectNumberSizeBytes model =
        if (isVersion4OrLater model) then 2 else 1

    let objectEntrySizeBytes model =
        if (isVersion4OrLater model) then 14 else 9

    let objectPropertyDefaultsAddress model = 
        let (ObjectTableAddress address) = objectTableAddress model
        ObjectPropertyDefaultsAddress address

    let objectPropertyDefaultAddress model (ObjectProperty property) =
        //let (ObjectPropertyNumber n') = n    
        if property < 1 || property > (objectPropertyCount model) then
            failwithf "Object property number out of range: %d. The valid range is %d to %d." property 1 (objectPropertyCount model)
        let (ObjectPropertyDefaultsAddress tableAddress) = objectPropertyDefaultsAddress model
        let (WordAddress address) = incrementWordAddressBy (property-1) (WordAddress tableAddress)
        ObjectPropertyAddress address

    let objectPropertyDefault model property =
        let (ObjectPropertyAddress address) = objectPropertyDefaultAddress model property
        readWord model (WordAddress address)

    let objectTreeAddress model = 
        let (ObjectTableAddress tableAddress) = objectTableAddress model
        let (WordAddress address) = incrementWordAddressBy (objectPropertyCount model) (WordAddress tableAddress)
        ObjectTreeAddress address

    (* 
       Guesses the number of objects by finding the lowest address of object properties and assuming that
       all object tree definitions are below that address. Given the start of the object tree and the
       size of each object entry, determine the object count. 
    *)
    let objectCount model =
        let offset = objectEntrySizeBytes model
        let rec loop lowPropAddr nextPropAddrPtr (nextObjNum, nextObjAddr) =
            let nextPropAddr = readWord model (WordAddress nextPropAddrPtr)
            let lowPropAddr' = System.Math.Min (lowPropAddr, nextPropAddr)
            if nextObjAddr >= lowPropAddr' then nextObjNum-1 else 
                loop lowPropAddr' (nextPropAddrPtr+offset) (nextObjNum+1, nextObjAddr+offset)
        let (ObjectTreeAddress object1Address) = objectTreeAddress model        
        let obj1PropertiesPtr = object1Address + (objectEntrySizeBytes model) - WordLength
        loop 0xFFFF obj1PropertiesPtr (2, obj1PropertiesPtr+WordLength)

    let allObjects model =
        let count = objectCount model
        [|1..count|] 
        |> Array.map Object

    let objectAddress model (Object obj) =
        if obj < 1 || obj > (objectCount model) then
            failwithf "Object number out of range: %d. The valid range is %d to %d." obj 1 (objectCount model)
        let (ObjectTreeAddress treeAddress) = objectTreeAddress model
        let (ByteAddress address) = (incrementByteAddressBy ((obj-1)*(objectEntrySizeBytes model)) (ByteAddress treeAddress))
        ObjectAddress address

    let objectAttributesAddress model obj =
        let (ObjectAddress address) = objectAddress model obj
        ObjectAttributesAddress address

    let objectAttributeAddress model obj (ObjectAttribute attribute) = 
        if attribute < 0 || attribute > ((objectAttributeCount model) - 1) then
            failwithf "Object attribute number out of range: %d. The valid range is %d to %d." attribute 0 (objectAttributeCount model)
        let (ObjectAttributesAddress address) = objectAttributesAddress model obj
        let (ByteAddress attributeAddress) = incrementByteAddressBy (attribute / 8)  (ByteAddress address)
        let bitNumber = BitNumber (7 - (attribute % 8))
        ObjectAttributeAddress (attributeAddress, bitNumber)

    let readObjectAttribute model obj attribute =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress model obj attribute
        readBit bitNumber (readByte model (ByteAddress address))

    let isObjectAttributeSet = readObjectAttribute

    let writeObjectAttribute model obj attribute isSet =
        let (ObjectAttributeAddress (address, bitNumber)) = objectAttributeAddress model obj attribute
        writeBit model (ByteAddress address) bitNumber isSet

    let setObjectAttribute model obj attribute = 
        writeObjectAttribute model obj attribute true

    let clearObjectAttribute model obj attribute = 
        writeObjectAttribute model obj attribute false

    let readAllObjectAttributes model obj =
        model
        |> allObjectAttributes
        |> Array.map (readObjectAttribute model obj)

    let readObjectNumber model (ObjectNumberAddress address) =
        if (isVersion4OrLater model) then
            readWord model (WordAddress address)
        else
            readByte model (ByteAddress address)
        |> Object

    let writeObjectNumber model (ObjectNumberAddress address) (Object obj) =
        if (isVersion4OrLater model) then
            writeWord model (WordAddress address) obj
        else
            writeByte model (ByteAddress address) obj

    let objectParentAddress model obj =
        let (ObjectAttributesAddress address) = objectAttributesAddress model obj
        ObjectNumberAddress (address + (objectAttributeSizeBytes model))

    let objectSiblingAddress model obj =
        let (ObjectNumberAddress parent) = objectParentAddress model obj
        ObjectNumberAddress (parent + (objectNumberSizeBytes model))

    let objectChildAddress model obj =
        let (ObjectNumberAddress sibling) = objectSiblingAddress model obj
        ObjectNumberAddress (sibling + (objectNumberSizeBytes model))

    let private readObjectRelation faddress model obj =
        readObjectNumber model (faddress model obj)

    let private writeObjectRelation faddress model obj relation =
        writeObjectNumber model (faddress model obj) relation

    let readObjectParent = 
        readObjectRelation objectParentAddress

    let readObjectSibling = 
        readObjectRelation objectSiblingAddress

    let readObjectChild = 
        readObjectRelation objectChildAddress

    let objectPropertiesPointer model obj =
        let (ObjectNumberAddress child) = objectChildAddress model obj
        WordAddress (child + objectNumberSizeBytes model)

    let objectPropertiesAddress model obj =
        ByteAddress (readWord model (objectPropertiesPointer model obj))

    let objectShortNameAddress = objectPropertiesAddress

    let objectPropertiesDataAddress model obj =
        let (ByteAddress address) = objectPropertiesAddress model obj
        let shortNameLengthWords = readByte model (ByteAddress address)
        ObjectPropertiesDataAddress (address + 1 + shortNameLengthWords*WordLength)

    let readObjectProperties model obj =
        let rec loop propAddr acc =
            let propAddr' = ByteAddress propAddr
            let size = readByte model propAddr'
            if size = 0 then acc else
            if (isVersion4OrLater model) then
                let propNum = readBits BitNumber5 BitCount6 size // Bottom 6 bits
                if (readBit BitNumber7 size) then                    
                    let dataSize = readBits BitNumber5 BitCount6 (readByte model (incrementByteAddress propAddr'))
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
                                                                 
        let (ObjectPropertiesDataAddress address) = objectPropertiesDataAddress model obj
        loop address []

    let objectShortName model obj =
        let (ByteAddress address) = objectShortNameAddress model obj
        Text.readZString model (ZStringAddress (address+1))

    let objectToString model obj =
        let (Object n) = obj
        let shortName = objectShortName model obj
        let (Object parent), (Object sibling), (Object child) = 
            readObjectParent model obj,
            readObjectSibling model obj,
            readObjectChild model obj
        let attributes = 
            model
            |> allObjectAttributes
            |> Array.choose (fun attr -> if (isObjectAttributeSet model obj attr) then Some attr else None)
            |> Array.map (fun (ObjectAttribute i) -> i)
         
        sprintf "%5d. %-35s[Parent: %5d] [Sibling: %5d] [Child: %5d] [Attributes: %A]" 
            n shortName parent sibling child attributes

    let showObject model obj = 
        printfn "%s" (objectToString model obj)
        
    let showObjects model =
        model
        |> allObjects
        |> Array.map (objectToString model)
        |> Array.iter (printfn "%s")
