#load "Type.fs"
#load "Utility.fs"
#load "Memory.fs"
#load "Machine.fs"

// The following updates the assembly search path
//#I __SOURCE_DIRECTORY__

open Zmac.Core

let testsRoot = @"" + __SOURCE_DIRECTORY__ + "/../../tests/zmac.Tests"
let storyFile = testsRoot + @"/Story/zork1.z3"
let machine = Machine.createFromFile storyFile
