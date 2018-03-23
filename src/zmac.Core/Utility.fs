namespace Zmac.Core

module Utility =

    open Zmac.Core.Type

    [<Literal>]
    let WordLength = 2

    // ZAddress manipulations
    let isAddressInRange (ByteAddress address) length =
        address >= 0 && address < length

    let isNotAddressInRange address length = 
        not (isAddressInRange address length)

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

    /// Creates an array of ByteAddress values across the range provided, from lowest to highest, which can be used to drive a pipeline.
    let rangeOfByteAddresses ((ByteAddress lowOffset), (ByteAddress highOffset)) =
        if (highOffset < lowOffset) then 
            failwithf "High address (%A) must not be less than low address (%A)" highOffset lowOffset
        [|lowOffset..highOffset|]
        |> Array.map ByteAddress

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

    /// Starting at the high bit, reads count bits from the given word and returns the result; that is, bits are read from rightmost to leftmost.
    let readBits (BitNumber high) (BitCount count) word =
        let shift = high - count + 1
        // We assume that we only ever read at most 8 bits, so using a byte for a mask should suffice
        let mask = 0xff >>> (8-count) 
        (word >>> shift) &&& mask

    let writeBit (BitNumber n) word isSet =
        let mask = 0x1 <<< n
        if isSet then
            word ||| mask
        else
            word &&& (~~~mask)

    let bytesToWord (hi:byte, lo:byte) =
        (int hi) <<< 8 ||| (int lo)

    let byteToWord (b:byte) = 
        bytesToWord (0uy, b)

    let byteToChar (b:byte) = char b

    let intToChar (i:int) = char i

    // // Divides the given array into two arrays at the specified address,
    // // with the first array containing the address.
    // let subdivideBytes (bs:byte array) address = 
    //     if (isAddressInRange address ((Array.length bs) - 1)) then
    //         let (ByteAddress address') = address
    //         bs.[..address'], bs.[address'+1..]
    //     else
    //         failwithf "%A is out of range for the buffer given" address   

    // let dereferenceByte (bs:byte array) address =
    //     if (isAddressInRange address (Array.length bs)) then
    //         let (ByteAddress address') = address
    //         bs.[address']
    //     else
    //         failwithf "%A is out of range for the buffer given" address   

    let charSeqToString (cs:char seq) =
        cs
        |> Seq.toArray
        |> System.String
        |> string

    let bin (tw: System.IO.TextWriter) value = 
        tw.Write("{0}", System.Convert.ToString(int64 value, 2))