namespace Zmac.Core

open Type
open Utility
open Machine
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
    let WordEntryEncodedTextLength_V3 = 4
    [<Literal>]
    let WordEntryEncodedTextLength_V4 = 6

    let wordEntryEncodedTextLength machine =
        if (isVersion4OrLater machine) then 
            WordEntryEncodedTextLength_V4
        else
            WordEntryEncodedTextLength_V3

    let wordSeparatorsCountAddress machine = 
        let (DictionaryAddress address) = dictionaryAddress machine
        address

    let wordSeparatorAddress machine i = 
        incrementByteAddressBy i (ByteAddress (wordSeparatorsCountAddress machine))

    let readWordSeparatorsCount machine =
        readByte machine (ByteAddress (wordSeparatorsCountAddress machine)) |> byteToInt

    let readWordSeparators machine =
        [|1..(readWordSeparatorsCount machine)|]
        |> Array.map (fun i -> (readByte machine (wordSeparatorAddress machine i)) |> byteToChar)
    
    let wordEntryLengthAddress machine =
        incrementByteAddressBy (readWordSeparatorsCount machine) (wordSeparatorAddress machine 1) 

    let readWordEntryLength machine =
        readByte machine (wordEntryLengthAddress machine) |> byteToInt

    let wordEntryCountAddress machine =
        incrementByteAddress (wordEntryLengthAddress machine) |> byteAddressToWordAddress

    let readWordEntryCount machine =
        readWord machine (wordEntryCountAddress machine)

    let wordEntriesAddress machine =
        incrementWordAddressBy 1 (wordEntryCountAddress machine) |> wordAddressToByteAddress

    let wordEntryAddress machine i =
        let (ByteAddress address) = wordEntriesAddress machine
        let offset = (i-1)*(readWordEntryLength machine)        
        ZStringAddress (address + offset)

    let wordEntry machine i =
        readZString machine (wordEntryAddress machine i)

    let wordEntryList machine =
        [1..(readWordEntryCount machine)]
        |> Seq.map (wordEntry machine)

    let showDictionary machine =
        machine
        |> wordEntryList
        |> Seq.mapi (fun i word -> sprintf "[%4d] %10s  " (i+1) word)
        |> Seq.chunkBySize 4
        |> Seq.map (Seq.fold (+) System.String.Empty)
        |> Seq.iter (fun line -> printfn "%s" line)