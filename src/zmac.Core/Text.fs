namespace Zmac.Core

open Type
open Utility
open Machine

module Text =

    type AlphabetId             = A0 | A1 | A2
    type Alphabet               = Alphabet of AlphabetId*char array

    // 3 standard character tables, each with 26 members
    let zCharA0 = [| 'a'..'z' |]
    let zCharA1 = [| 'A'..'Z' |]
    let zCharA2 = 
        [| 
            char 0; '\n'; 
            '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 
            '.'; ','; '!'; '?'; '_'; '#'; '''; '"'; '/'; '\\'; '-'; ':'; '('; ')' 
        |]

    // Special Z-Characters 
    let zCharSpace                      = 0             // equivalent to ASCII space
    let zAbbreviationReferenceV3        = [ 1; 2; 3 ]   // indicates an abbreviation should be inserted
    let zCharZsciiCodeFollows           = 6             // indicates that a ZSCII code follows

    // At the given address, reads encoded characters until a terminator bit is set, returning the
    // ordered list of codes. This sequence of codes must be converted to Z-Characters.
    let readZCharCodeSeq machine (ZStringAddress address) =

        // Structure of a double-byte word representing 3 Z-Characters
        let zCharOffset         = [ BitNumber10; BitNumber5; BitNumber0 ]
        let zCharTerminator     = BitNumber15
        let zCharLength         = BitCount5

        let rec loop addr acc =
            let word = readWord machine addr
            let isEndOfString = readBit zCharTerminator word
            let zcodes = 
                zCharOffset 
                |> List.map (fun offset -> readBits offset zCharLength word)
                |> List.append acc
            if isEndOfString then 
                zcodes
            else 
                loop (incrementWordAddress addr) zcodes                
        loop (WordAddress address) []

    // Given the hi- and lo-order 5-bit code comprising a ZSCII character, returns the mapped character
    let lookupZsciiCode (hi, lo) =        
        let code = 32*hi + lo |> byte
        System.Text.Encoding.ASCII.GetChars([| code |]).[0]
    
    (* 
        Abbreviations are referenced by a table of addresses. These addresses must be doubled
        and then point to Z-encoded text representing the abbreviation.
    *)
    let abbreviationAddress machine (Abbreviation i) =        
        // Determine the offset into the table that contains our "packed" abbreviation address
        let (AbbreviationsTableAddress tableAddress) = abbreviationsTableAddress machine
        AbbreviationAddress ((readWord machine (incrementWordAddressBy i (WordAddress tableAddress)))*2)

    let readZString machine address =
        let zcodes = readZCharCodeSeq machine address
        
        let rec loop zcodes acc =
            //printfn "In: %A, Acc: %A" zstr acc
            match zcodes with
            | [] -> acc // to string?
            | z::zs when z = zCharSpace -> 
                //printfn "[Space]"
                loop zs (List.append acc [' '])
            | z1::z2::zs when (z1 = 1 || z1 = 2 || z1 = 3) ->
                printfn "[Abbrev]" 
                //let abbrIndex = 32 * (z1 - 1) + z2
                // look up abbrv
                loop zs acc@['?'] // need to expand out the abbr
            | z1::z2::zs when z1 = 4 && z2 > 5 -> 
                //printfn "[A1:%d]" z2
                loop zs (List.append acc [zCharA1.[z2-6]])
            | z1::z2::zs when z1 = 5 && z2 > 6 -> 
                //printfn "[A2:%d]" z2
                loop zs (List.append acc [zCharA2.[z2-6]])
            | z1::z2::z3::z4::zs when (z1 = 5 && z2 = 6) -> 
                //printfn "[ASCII:%d,%d]" z3 z4
                let zchar = lookupZsciiCode (z3, z4)
                loop zs (List.append acc [zchar])
            | z1::z2::zs when (z1 = 4 || z1 = 5) && (z2 = 4 || z2 = 5) -> 
                loop zs acc
            | [z] when z = 4 || z = 5 -> 
                //printfn "Done: %A" acc
                acc
            | z::zs when z > 5 -> 
                //printfn "[A0:%d]" z; 
                loop zs (List.append acc [zCharA0.[z-6]])
            | _::zs -> 
                //printfn "[Pad]"; 
                loop zs acc
        
        loop zcodes []
        |> List.toArray
        |> (System.String >> string)

    let readAbbreviation machine abbreviation =
        let (AbbreviationAddress address) = abbreviationAddress machine abbreviation
        readZString machine (ZStringAddress address)