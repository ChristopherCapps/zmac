namespace Zmac.Core

open Type

module External =

    (* 
        A structure representing mouse input:
        y-coordinate
        x-coordinate
        buttons: An array of booleans indicating activation of 16 buttons, with the primary & secondary buttons given first
        menu: A tuple indicating the menu and item selected
    *)
    type MouseInput = {
        y: uint16
        x: uint16
        buttons: bool array
        menu: byte * byte
    }

    type T = {
        readMouse: unit -> MouseInput
    }
    