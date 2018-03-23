namespace Zmac.Core

open Type
open Utility
open Model
open Text

module Dictionary =

(* 
    The Z-Machine Dictionary layout is as follows:
    1-byte      Number of word separator characters which follow
    n-bytes     Word separator characters, as ASCII chars
    1-byte      Size of each word entry in the dictionary, call it "w"
    1-word      Number of entries in the dictionary

    In versions before Version 4:
    4-bytes     An encoding of 6 Z-characters
    w-4 bytes   "Data" to be used by the game

    In versions 4 and later:
    6-bytes     An encoding of 9 Z-characters
    w-4 bytes   "Data" to be used by the game
*)

    [<Literal>]
    let DictionaryEntryEncodedTextLength_V3 = 4
    [<Literal>]
    let DictionaryEntryEncodedTextLength_V4 = 6

    let dictionaryEntryEncodedTextLength model =
        if isVersion4OrLater model then 
            DictionaryEntryEncodedTextLength_V4
        else
            DictionaryEntryEncodedTextLength_V3

    let wordSeparatorsCountAddress model = 
        let (DictionaryAddress address) = dictionaryAddress model
        address

    let wordSeparatorAddress model i = 
        incrementByteAddressBy i (ByteAddress (wordSeparatorsCountAddress model))

    let wordSeparatorsCount model =
        readByte model (ByteAddress (wordSeparatorsCountAddress model))

    let wordSeparators model =
        [|1..wordSeparatorsCount model|]
        |> Array.map (fun i -> char (readByte model (wordSeparatorAddress model i)))
    
    let dictionaryEntryLengthAddress model =
        incrementByteAddressBy (wordSeparatorsCount model) (wordSeparatorAddress model 1) 

    let dictionaryEntryLength model =
        readByte model (dictionaryEntryLengthAddress model)

    let dictionaryEntryCountAddress model =
        incrementByteAddress (dictionaryEntryLengthAddress model) |> byteAddressToWordAddress

    let dictionaryEntryCount model =
        readWord model (dictionaryEntryCountAddress model)

    let dictionaryEntriesAddress model =
        incrementWordAddressBy 1 (dictionaryEntryCountAddress model) |> wordAddressToByteAddress

    let isDictionaryEntryInRange model (DictionaryEntry i) =
        i >= 1 && i <= (dictionaryEntryCount model)

    let dictionaryEntryAddress model (DictionaryEntry i) =
        if isDictionaryEntryInRange model (DictionaryEntry i) then
            let (ByteAddress address) = dictionaryEntriesAddress model
            let offset = (i-1) * dictionaryEntryLength model
            DictionaryEntryAddress (address + offset)
        else
            failwithf "Invalid dictionary entry reference: %d. The valid range is %d to %d." i 1 (dictionaryEntryCount model)            

    let dictionaryEntryWord model i = 
        let (DictionaryEntryAddress dictionaryEntryBase) = dictionaryEntryAddress model i
        readZString model (ZStringAddress dictionaryEntryBase)

    // let dictionaryEntryData model i =
    //     let (DictionaryEntryAddress dictionaryEntryBase) = dictionaryEntryAddress model i
    //     let dictionaryEntryDataAddress = incrementByteAddressBy (dictionaryEntryLength model) dictionaryEntryBase
    //     // need to fetch bytes

    let allDictionaryEntries model =
        let entryCount = dictionaryEntryCount model
        [|1..entryCount|]
        |> Array.map DictionaryEntry

    let allDictionaryEntryWords model =
        model
        |> allDictionaryEntries
        |> Array.map (dictionaryEntryWord model)

    let showDictionary model =
        model
        |> allDictionaryEntryWords
        |> Array.mapi (fun i word -> sprintf "[%4d] %10s  " (i+1) word)
        |> Array.chunkBySize 4
        |> Array.map (Seq.fold (+) System.String.Empty)
        |> Array.iter (fun line -> printfn "%s" line)