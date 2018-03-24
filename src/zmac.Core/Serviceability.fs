namespace Zmac.Core

open Type

module Serviceability =

    module Printf =

        type TextWriter = System.IO.TextWriter

        let bin (tw: TextWriter) value = 
            System.Convert.ToString(int64 value, 2)
            |> tw.Write

        module Dictionary = 

            let printEntry (tw:TextWriter) (model, entry) =
                let (DictionaryEntry number) = entry
                tw.Write(sprintf "[%4d] '%s'" number (Dictionary.dictionaryEntryWord model entry))

            let printEntries (tw:TextWriter) (model, columns) =            
                model
                |> Dictionary.allDictionaryEntryWords
                |> Seq.mapi (fun i word -> sprintf "[%4d] %10s  " (i+1) word)
                |> Seq.chunkBySize columns
                |> Seq.map (Seq.fold (+) System.String.Empty)
                |> Seq.fold (fun lines line -> sprintf "%s\n%s" lines line) System.String.Empty
                |> tw.Write