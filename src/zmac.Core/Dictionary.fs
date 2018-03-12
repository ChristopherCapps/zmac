namespace Zmac.Core

open Type
open Utility
open Machine
open Machine.Memory
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

    let dictionaryEntryEncodedTextLength machine =
        if isVersion4OrLater machine then 
            DictionaryEntryEncodedTextLength_V4
        else
            DictionaryEntryEncodedTextLength_V3

    let wordSeparatorsCountAddress machine = 
        let (DictionaryAddress address) = dictionaryAddress machine
        address

    let wordSeparatorAddress machine i = 
        incrementByteAddressBy i (ByteAddress (wordSeparatorsCountAddress machine))

    let wordSeparatorsCount machine =
        readByte machine (ByteAddress (wordSeparatorsCountAddress machine))

    let wordSeparators machine =
        [|1..wordSeparatorsCount machine|]
        |> Array.map (fun i -> char (readByte machine (wordSeparatorAddress machine i)))
    
    let dictionaryEntryLengthAddress machine =
        incrementByteAddressBy (wordSeparatorsCount machine) (wordSeparatorAddress machine 1) 

    let dictionaryEntryLength machine =
        readByte machine (dictionaryEntryLengthAddress machine)

    let dictionaryEntryCountAddress machine =
        incrementByteAddress (dictionaryEntryLengthAddress machine) |> byteAddressToWordAddress

    let dictionaryEntryCount machine =
        readWord machine (dictionaryEntryCountAddress machine)

    let dictionaryEntriesAddress machine =
        incrementWordAddressBy 1 (dictionaryEntryCountAddress machine) |> wordAddressToByteAddress

    let isDictionaryEntryInRange machine (DictionaryEntry i) =
        i >= 1 && i <= dictionaryEntryCount machine

    let dictionaryEntryAddress machine (DictionaryEntry i) =
        if isDictionaryEntryInRange machine (DictionaryEntry i) then
            let (ByteAddress address) = dictionaryEntriesAddress machine
            let offset = (i-1) * dictionaryEntryLength machine
            ZStringAddress (address + offset)
        else
            failwithf "Invalid dictionary entry reference: %d. The valid range is %d to %d." i 1 (dictionaryEntryCount machine)            

    let dictionaryEntry machine i = 
        readZString machine (dictionaryEntryAddress machine i)

    let dictionaryEntryList machine =
        [1..dictionaryEntryCount machine]
        |> Seq.map (DictionaryEntry >> dictionaryEntry machine)

    let showDictionary machine =
        machine
        |> dictionaryEntryList
        |> Seq.mapi (fun i word -> sprintf "[%4d] %10s  " (i+1) word)
        |> Seq.chunkBySize 4
        |> Seq.map (Seq.fold (+) System.String.Empty)
        |> Seq.iter (fun line -> printfn "%s" line)