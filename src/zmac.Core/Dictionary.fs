namespace Zmac.Core

open Type
open Utility
open Machine

module Dictionary =

    let wordSeparatorsCountAddress machine = 
        let (DictionaryAddress address) = dictionaryAddress machine
        address

    let wordSeparatorAddress machine i = 
        incrementByteAddressBy i (wordSeparatorsCountAddress machine)

    let readWordSeparatorsCount machine =
        readByte machine (wordSeparatorsCountAddress machine) |> byteToInt

    let readWordSeparators machine =
        [|1..(readWordSeparatorsCount machine)|]
        |> Array.map (fun i -> (readByte machine (wordSeparatorAddress machine i)) |> byteToChar)
    
    let wordEntryLengthAddress machine =
        incrementByteAddressBy (readWordSeparatorsCount machine) (wordSeparatorsCountAddress machine) 

    let readWordEntryLength machine =
        readByte machine (wordEntryLengthAddress machine) |> byteToInt

    let wordEntryCountAddress machine =
        incrementByteAddress (wordEntryLengthAddress machine) |> byteAddressToWordAddress

    let readWordEntryCount machine =
        

    let wordEntriesAddress machine =
        incrementByteAddressBy WordLength (wordEntryCountAddress machine)

    let wordEntryAddress machine i =
        incrementByteAddressBy 
        