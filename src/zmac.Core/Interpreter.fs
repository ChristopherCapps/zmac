namespace Zmac.Core

module Interpreter =

    type T = {
        machine: Machine.T
        external: External.T
    }

    //let make machine external = { machine; external }
