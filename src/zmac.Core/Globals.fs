namespace Zmac.Core

open Type
open Utility
open Machine

module Globals =

    [<Literal>]
    let FirstGlobalVariable         = 0x10
    [<Literal>]
    let LastGlobalVariable          = 0xFF

    let globalVariableAddress machine (GlobalVariable n) =
        if n >= FirstGlobalVariable && n <= LastGlobalVariable then
            let (GlobalVariablesTableAddress address) = globalVariablesTableAddress machine
            incrementWordAddressBy (n - FirstGlobalVariable) (WordAddress address)
        else
            failwithf "Invalid global variable reference: %d. The valid range is %d to %d." n FirstGlobalVariable LastGlobalVariable

    let readGlobalVar machine n =
        readWord machine (globalVariableAddress machine n)

    let writeGlobalVar machine n value =
        writeWord machine (globalVariableAddress machine n) value

    let globalVarArray machine =
        [|FirstGlobalVariable..LastGlobalVariable|]
        |> Array.map (GlobalVariable >> readGlobalVar machine)

    let showGlobalVariables machine =
        machine
        |> globalVarArray
        |> Seq.mapi (fun i var -> sprintf "[%3d] %-7d" (i+0x10) var)
        |> Seq.iter (fun line -> printfn "%s" line)