namespace Zmac.Core

module Type =

    // An absolute Z-Machine address which can span the entire machine
    type ByteAddress = ByteAddress of int
    type WordAddress = WordAddress of int

    type VersionAddress = VersionAddress of ByteAddress
    type StaticMemoryAddress = StaticMemoryAddress of ByteAddress
    type DictionaryAddress = DictionaryAddress of ByteAddress

    // A typed reference to a specific bit in a byte or word
    type BitNumber = BitNumber of int

    let BitNumber0 = BitNumber 0
    let BitNumber1 = BitNumber 1
    let BitNumber2 = BitNumber 2
    let BitNumber3 = BitNumber 3
    let BitNumber4 = BitNumber 4
    let BitNumber5 = BitNumber 5
    let BitNumber6 = BitNumber 6
    let BitNumber7 = BitNumber 7

    // A typed reference to a number of consecutive bits
    type BitCount = BitCount of int
    
    let BitCount1 = BitCount 1
    let BitCount2 = BitCount 2
    let BitCount3 = BitCount 3
    let BitCount4 = BitCount 4
    let BitCount5 = BitCount 5
    let BitCount6 = BitCount 6
    let BitCount7 = BitCount 7
    let BitCount8 = BitCount 8

    type Version = Version1 | Version2 | Version3 | Version4 | Version5 | Version6