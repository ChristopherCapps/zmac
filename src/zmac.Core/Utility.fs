namespace Zmac.Core

module Utility =

    open Zmac.Core.Type

    [<Literal>]
    let WordLength = 2

    // ZAddress manipulations
    let isAddressInRange (ByteAddress address) length =
        address >= 0 && address < length

    let incrementByteAddressBy count (ByteAddress address) =
        ByteAddress (address+count)

    let incrementByteAddress =
        incrementByteAddressBy 1

    let decrementByteAddressBy count (ByteAddress address) =
        ByteAddress (address-count)

    let decrementByteAddress =
        decrementByteAddressBy 1

    let incrementWordAddressBy count (WordAddress address) =
        WordAddress (address+count*WordLength)

    let incrementWordAddress = 
        incrementWordAddressBy 1

    let getHiByteAddress (WordAddress address) =
        ByteAddress address

    let getLoByteAddress (WordAddress address) =
        ByteAddress (address+1)

    let byteAddressToWordAddress (ByteAddress address) =
        WordAddress address

    let wordAddressToByteAddress (WordAddress address) =
        ByteAddress address

    let readBit (BitNumber n) word =
        ((word &&& (0x1 <<< n)) >>> n) = 1

    let readBits (BitNumber high) (BitCount count) word =
        (((0xff >>> (8-count)) <<< high) &&& word) >>> high

    let bytesToWord (hi:byte, lo:byte) =
        (int hi) <<< 8 ||| (int lo)

    let byteToChar (b:byte) = char b

    let byteToInt (b:byte) = int b

    let intToChar (i:int) = char i

    // Divides the given array into two arrays at the specified address,
    // with the first array containing the address.
    let subdivideBytes (bs:byte array) address = 
        if (isAddressInRange address ((Array.length bs) - 1)) then
            let (ByteAddress address') = address
            bs.[..address'], bs.[address'+1..]
        else
            failwithf "%A is out of range for the buffer given" address   

    let dereferenceByte (bs:byte array) address =
        if (isAddressInRange address (Array.length bs)) then
            let (ByteAddress address') = address
            bs.[address']
        else
            failwithf "%A is out of range for the buffer given" address   