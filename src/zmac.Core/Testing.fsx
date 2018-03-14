#load "Type.fs"
#load "Utility.fs"
#load "Memory.fs"
#load "Machine.fs"
#load "Globals.fs"
#load "Text.fs"
#load "Dictionary.fs"
#load "Object.fs"

// The following updates the assembly search path
//#I __SOURCE_DIRECTORY__

open Zmac.Core

let testsRoot = @"" + __SOURCE_DIRECTORY__ + "/../../tests/zmac.Tests"
let storyFile = testsRoot + @"/Story/zork1.z3"
let machine = Machine.Helpers.createFromFile storyFile
let machine4 = Machine.Helpers.createFromFile (testsRoot + @"/Story/Trinity.z4")

open Zmac.Core.Type
open Zmac.Core.Utility
open Zmac.Core.Machine
open Zmac.Core.Globals
open Zmac.Core.Text
open Zmac.Core.Dictionary
open Zmac.Core.Object

//dictionaryEntry machine (DictionaryEntry 10)
