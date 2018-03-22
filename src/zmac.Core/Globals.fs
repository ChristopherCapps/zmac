namespace Zmac.Core

open Type
open Utility
open Story

(*
    Globals are 2-byte words numbered from $10 to $FF, so there are 240 values. They
    are stored in a table which cannot be relocated.
*)
module Globals =

    /// the smallest valid global variable number
    [<Literal>]
    let FirstGlobalVariable         = 0x10
    /// the largest valid global variable number
    [<Literal>]
    let LastGlobalVariable          = 0xFF

    ///<summary>Returns the address of the global variable whose number is given, from the specified story.</summary>
    /// <param name="story">the Z-story to reference</param>
    /// <param name="n">the global variable number, from <c>$10</c> to <c>$ff</c></param>
    let globalVariableAddress story (GlobalVariable n) =
        if n >= FirstGlobalVariable && n <= LastGlobalVariable then
            let (GlobalVariablesTableAddress address) = globalVariablesTableAddress story
            incrementWordAddressBy (n - FirstGlobalVariable) (WordAddress address)
        else
            failwithf "Invalid global variable reference: %d. The valid range is %d to %d." n FirstGlobalVariable LastGlobalVariable

    /// Reads and returns the 2-byte word value of the global variable whose number is given, from the specified story. 
    /// <param name="story">the Z-story to reference</param>
    /// <param name="n">the global variable number, from <c>$10</c> to <c>$ff</c></param>
    /// <returns>a 2-byte word value</returns>
    let readGlobalVar story n =
        readWord story (globalVariableAddress story n)

    /// Updates the 2-byte value of the specified global variable in the given Z-story, returning an updated story.
    /// <param name="story">the Z-story to reference</param>
    /// <param name="n">the global variable number, from <c>$10</c> to <c>$ff</c></param>
    /// <param name="value">the new value of the global variable</param>
    /// <returns>a new Z-story containing the update</returns>
    let writeGlobalVar story n value =
        writeWord story (globalVariableAddress story n) value

    let allGlobalVariables story =
        [|FirstGlobalVariable..LastGlobalVariable|]
        |> Array.map (GlobalVariable >> readGlobalVar story)

    let showGlobalVariables story =
        story
        |> allGlobalVariables
        |> Array.mapi (fun i var -> sprintf "[%3d] %-7d" (i+0x10) var)
        |> Array.iter (fun line -> printfn "%s" line)