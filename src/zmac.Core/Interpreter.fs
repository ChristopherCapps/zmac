namespace Zmac.Core

module Interpreter =

    type T = {
        story: Story.T
        external: External.T
    }

    //let make story external = { story; external }
