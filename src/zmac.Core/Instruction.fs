namespace Zmac.Core

open Zmac.Core.Type
open Zmac.Core.Utility

module Instruction =

    type Operand = 
        | LargeConst of int
        | SmallConst of int
        | Variable of int
        | Omitted

    type OpCodeForm = 
        | Long
        | Short
        | Extended
        | Variable

    type OpCount = Op0 | Op1 | Op2 | Var