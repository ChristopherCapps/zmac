namespace Zmac.Core

module Interpreter =

    type T = {
        model: Model.T
        external: External.T
    }

    //let make model external = { model; external }
