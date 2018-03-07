namespace Zmac.Core

open Type
open Utility
open Machine

module Text =

    type AlphabetId             = A0 | A1 | A2
    type Alphabet               = Alphabet of AlphabetId*char array

    [<Literal>]
    let AbbreviationsCount      = 96

    // 3 standard character tables, each with 26 members; Version 1 has a different A2
    let AlphabetA0              = Alphabet (A0, [| 'a'..'z' |])
    let AlphabetA1              = Alphabet (A1, [| 'A'..'Z' |])    
    let AlphabetA2              = Alphabet (A2, Seq.toArray " \n0123456789.,!?_#\'/\"\\-:()")

    // Special Z-Codes which do not directly represent Alphabet characters but are instructions
    let InsertSpaceCharacter            = 0             // code equivalent to ASCII space    
    let InsertAbbreviationFromSet1      = 1             // lookup an abbreviation in set 1 using indexer to follow
    let InsertAbbreviationFromSet2      = 2             // lookup an abbreviation in set 2 using indexer to follow
    let InsertAbbreviationFromSet3      = 3             // lookup an abbreviation in set 3 using indexer to follow
    let ShiftOnceToA1                   = 4             // next character should be looked up in Alphabet A1
    let ShiftOnceToA2                   = 5             // next character should be looked up in Alphabet A2
    let InsertZsciiCode                 = 6             // indicates that a 10-bit ZSCII code follows this code
    let FirstZCharacterIndex            = 6             // Z-code corresponding to first Z-character index in an Alphabet

    let InsertAbbreviationRequest = [InsertAbbreviationFromSet1; InsertAbbreviationFromSet2; InsertAbbreviationFromSet3]

    // Simple helper for internal use that makes matching operations clearer
    let isAlphabetCharacterIn alphabet ch = 
        match alphabet with
        | A0 | A1 -> ch > ShiftOnceToA2 // 5
        | A2 -> ch > InsertZsciiCode // 6

    // Fetches the character associated with the given code in the provided Alphabet
    let readAlphabetCharacterIn (Alphabet (_, chars)) code = 
        let index = code - FirstZCharacterIndex // the code is offset from the Alphabet index
        if index >= 0 && index < 26 then
            chars.[index]
        else
            failwithf "Illegal range for Alphabet character index: %d" index

    // At the given address, reads encoded characters until a terminator bit is set, returning the
    // ordered list of codes. This sequence of codes must be converted to Z-Characters.
    let readZCodeSeq machine (ZStringAddress address) =

        // These define the structure of a double-byte word representing 3 Z-Codes; they are 
        // only relevant for parsing the sequence of codes and so are declared here
        let ZCodeOffsets        = [ BitNumber10; BitNumber5; BitNumber0 ]
        let ZStringTerminator   = BitNumber15
        let ZCodeLength         = BitCount5

        // Recursively parse and collect zcodes from the given address until <eos> bit is set
        let rec loop addr acc =
            let word = readWord machine addr
            let isEndOfString = readBit ZStringTerminator word
            let zcodes = 
                ZCodeOffsets
                |> List.map (fun offset -> readBits offset ZCodeLength word)
                |> List.append acc
            if isEndOfString then 
                zcodes
            else 
                loop (incrementWordAddress addr) zcodes                
        loop (WordAddress address) []

    // Given the hi- and lo-order 5-bit code comprising a ZSCII character, returns the mapped character.
    // See Table 1 (http://inform-fiction.org/zmachine/standards/z1point1/sect03.html) for Unicode translations
    // and customization for Version 5 and later
    let lookupZsciiCode (hi, lo) =        
        let code = 32*hi + lo |> byte
        match code with
        | 9uy -> Some '\t' // tab
        | 11uy -> Some ' '
        | 13uy -> Some '\n'
        | ch when ch >= 32uy && ch <= 126uy -> // Standard ASCII
            Some (System.Text.Encoding.ASCII.GetChars([| code |]).[0])
        | ch when ch >= 155uy && ch <= 223uy // Should be converted to 16-bit Unicode
            -> Some '?' 
        | _ -> None
     
    (* 
        Abbreviations are referenced by a table of addresses. These addresses must be doubled
        and then point to Z-encoded text representing the abbreviation.
    *)
    let abbreviationAddress machine (Abbreviation i) =        
        // Determine the offset into the table that contains our "packed" abbreviation address
        let (AbbreviationsTableAddress tableAddress) = abbreviationsTableAddress machine
        AbbreviationAddress ((readWord machine (incrementWordAddressBy i (WordAddress tableAddress)))*2)

    // TODO: Consider refactoring such that we read a single ZChar in a separate function; note
    // this will complicate the recursive abbreviation insertion
    let rec readZString machine address =
        //let version = version machine

        let readAbbreviationAsChars abbreviationSet abbreviationIndex =
            let abbreviation = Abbreviation (32 * (abbreviationSet - 1) + abbreviationIndex)
            let (AbbreviationAddress abbreviationAddress) = abbreviationAddress machine abbreviation
            readZString machine (ZStringAddress abbreviationAddress)
            |> Seq.toList
        
        let rec loop zcodes acc =
            // We've processed all the input codes and are
            //printfn "In: %A, Acc: %A" zstr acc
            match zcodes with
            | [] -> acc

            // ZCode instruction to insert a space
            | ch::zs when ch = InsertSpaceCharacter -> 
                //printfn "[Space]"
                loop zs (acc @ [' '])

            // ZCode sequence to insert an "abbreviation" given its set and index into that set, post Version 1
            | abbreviationSet::abbreviationIndex::zs 
                when List.contains abbreviationSet InsertAbbreviationRequest ->
                    //printfn "[Abbreviation: %s]" zstring
                    loop zs (acc @ (readAbbreviationAsChars abbreviationSet abbreviationIndex))

            // ZCode sequence to insert a character from A1, but only if 'ch' is an actual character and not a special instruction
            | shiftAlphabetOnce::ch::zs when shiftAlphabetOnce = ShiftOnceToA1 && (ch |> isAlphabetCharacterIn A1) -> 
                //printfn "[A1:%d]" z2
                loop zs (acc @ [readAlphabetCharacterIn AlphabetA1 ch])

            // ZCode sequence to insert a character from A2, but only if 'ch' is an actual character and not a special instruction
            | shiftAlphabetOnce::ch::zs when shiftAlphabetOnce = ShiftOnceToA2 && (ch |> isAlphabetCharacterIn A2) -> 
                //printfn "[A2:%d]" z2
                loop zs (acc @ [readAlphabetCharacterIn AlphabetA2 ch])

            // ZCode sequence to insert a ZSCII character given hi- and lo-order 5-bit chunks
            // TODO: Need to support an alternate alphabet table for V5 and later
            | shiftAlphabetOnce::alphabetA2Instruction::zsciiHi::zsciiLo::zs 
                when (shiftAlphabetOnce = ShiftOnceToA2 && alphabetA2Instruction = InsertZsciiCode) -> 
                    //printfn "[ASCII:%d,%d]" z3 z4
                    match lookupZsciiCode (zsciiHi, zsciiLo) with
                    | Some ch -> loop zs (acc @ [ch])
                    | None -> loop zs acc

            // ZCode sequence that shifts the Alphabet in order to pad the string; ignore
            | shiftAlphabet1::shiftAlphabet2::zs 
                when (shiftAlphabet1 = ShiftOnceToA1 || shiftAlphabet1 = ShiftOnceToA2) && 
                    (shiftAlphabet2 = ShiftOnceToA1 || shiftAlphabet2 = ShiftOnceToA2) -> 
                        loop zs acc // ignore and continue

            // Odd-length string that ends with a single Alphabet shift; ignore
            | [shiftAlphabet] when shiftAlphabet = ShiftOnceToA1 || shiftAlphabet = ShiftOnceToA2 -> 
                //printfn "Done: %A" acc
                acc // ignore and continue

            // Finally we have a normal request to insert a character from the default A0
            | ch::zs when ch |> isAlphabetCharacterIn A0 -> 
                //printfn "[A0:%d]" z; 
                loop zs (acc @ [readAlphabetCharacterIn AlphabetA0 ch])

            // Catch-all for missed patterns that should (hopefully) be ignored
            | _::zs -> 
                //printfn "[Pad]"; 
                loop zs acc // ignore
        
        loop (readZCodeSeq machine address) List.empty
        |> charSeqToString

    let readAbbreviation machine abbreviation =
        let (AbbreviationAddress address) = abbreviationAddress machine abbreviation
        readZString machine (ZStringAddress address)

    let abbreviationsList machine =
        [0..(AbbreviationsCount-1)]
        |> Seq.map (fun i -> readAbbreviation machine (Abbreviation i))

    let showAbbreviations machine =
        machine
        |> abbreviationsList
        |> Seq.mapi (fun i abbr -> sprintf "[%4d] \"%s\"" i abbr)
        |> Seq.iter (fun line -> printfn "%s" line)