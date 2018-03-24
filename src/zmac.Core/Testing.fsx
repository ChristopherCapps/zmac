#load "Type.fs"
#load "Utility.fs"
#load "Memory.fs"
#load "Model.fs"
#load "Globals.fs"
#load "Text.fs"
#load "Dictionary.fs"
#load "Object.fs"
#load "Locals.fs"
#load "Instruction.fs"
#load "Story.fs"
#load "External.fs"
#load "Interpreter.fs"
#load "Serviceability.fs"

// The following updates the assembly search path
//#I __SOURCE_DIRECTORY__

open Zmac.Core

let testsRoot = @"" + __SOURCE_DIRECTORY__ + "/../../tests/zmac.Tests"
let storyFile = (testsRoot + @"/Story/zork1.z3")
let storyFile4 = (testsRoot + @"/Story/Trinity.z4")
let story = Story.ofFile storyFile
let story4 = Story.ofFile storyFile4

open Zmac.Core.Type
open Zmac.Core.Utility
open Zmac.Core.Model
open Zmac.Core.Globals
open Zmac.Core.Text
open Zmac.Core.Dictionary
open Zmac.Core.Object
open Zmac.Core.Locals
open Zmac.Core.Instruction
open Zmac.Core.Serviceability.Printf

//dictionaryEntry model (DictionaryEntry 10)
let instr = decode story (InstructionAddress 0x4f05)
display story instr