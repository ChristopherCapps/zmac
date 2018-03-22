namespace Zmac.Core

open Type
open Utility
open Story
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

    let dictionaryEntryEncodedTextLength story =
        if isVersion4OrLater story then 
            DictionaryEntryEncodedTextLength_V4
        else
            DictionaryEntryEncodedTextLength_V3

    let wordSeparatorsCountAddress story = 
        let (DictionaryAddress address) = dictionaryAddress story
        address

    let wordSeparatorAddress story i = 
        incrementByteAddressBy i (ByteAddress (wordSeparatorsCountAddress story))

    let wordSeparatorsCount story =
        readByte story (ByteAddress (wordSeparatorsCountAddress story))

    let wordSeparators story =
        [|1..wordSeparatorsCount story|]
        |> Array.map (fun i -> char (readByte story (wordSeparatorAddress story i)))
    
    let dictionaryEntryLengthAddress story =
        incrementByteAddressBy (wordSeparatorsCount story) (wordSeparatorAddress story 1) 

    let dictionaryEntryLength story =
        readByte story (dictionaryEntryLengthAddress story)

    let dictionaryEntryCountAddress story =
        incrementByteAddress (dictionaryEntryLengthAddress story) |> byteAddressToWordAddress

    let dictionaryEntryCount story =
        readWord story (dictionaryEntryCountAddress story)

    let dictionaryEntriesAddress story =
        incrementWordAddressBy 1 (dictionaryEntryCountAddress story) |> wordAddressToByteAddress

    let isDictionaryEntryInRange story (DictionaryEntry i) =
        i >= 1 && i <= (dictionaryEntryCount story)

    let dictionaryEntryAddress story (DictionaryEntry i) =
        if isDictionaryEntryInRange story (DictionaryEntry i) then
            let (ByteAddress address) = dictionaryEntriesAddress story
            let offset = (i-1) * dictionaryEntryLength story
            DictionaryEntryAddress (address + offset)
        else
            failwithf "Invalid dictionary entry reference: %d. The valid range is %d to %d." i 1 (dictionaryEntryCount story)            

    let dictionaryEntryWord story i = 
        let (DictionaryEntryAddress dictionaryEntryBase) = dictionaryEntryAddress story i
        readZString story (ZStringAddress dictionaryEntryBase)

    // let dictionaryEntryData story i =
    //     let (DictionaryEntryAddress dictionaryEntryBase) = dictionaryEntryAddress story i
    //     let dictionaryEntryDataAddress = incrementByteAddressBy (dictionaryEntryLength story) dictionaryEntryBase
    //     // need to fetch bytes

    let allDictionaryEntries story =
        let entryCount = dictionaryEntryCount story
        [|1..entryCount|]
        |> Array.map DictionaryEntry

    let allDictionaryEntryWords story =
        story
        |> allDictionaryEntries
        |> Array.map (dictionaryEntryWord story)

    let showDictionary story =
        story
        |> allDictionaryEntryWords
        |> Array.mapi (fun i word -> sprintf "[%4d] %10s  " (i+1) word)
        |> Array.chunkBySize 4
        |> Array.map (Seq.fold (+) System.String.Empty)
        |> Array.iter (fun line -> printfn "%s" line)